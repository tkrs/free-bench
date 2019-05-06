import sbt._

import scala.annotation.tailrec

object Algebras {

  val templates: Seq[Template] = List(
    GenAlgebra,
    GenAlgebras0,
    GenPrograms,
    GenRuns,
    GenIdKBench,
  )

  def gen(dir: File) = for (t <- templates) yield {
    val tgtFile = t.file(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxAlgebras = 10

  class TemplateVals(val num: Int) {
    val nums = 0.to(num)
  }

  trait Template {
    def file(root: File): File
    def content(tv: TemplateVals): String
    def body: String = content(new TemplateVals(maxAlgebras))
  }

  object GenAlgebra extends Template {
    def file(root: File) = root / "algebras" / "Algebras.scala"
    def content(tv: TemplateVals): String = {
      import tv._

      val b = nums.map { n =>
       s"""
           |sealed trait A$n[A]
           |object A$n {
           |  final case class Foo(value: Int) extends A$n[Unit]
           |  type _a$n[R] = A$n |= R
           |}
           |""".stripMargin
      }
      val h =
        s"""package algebras
           |
           |import _root_.org.atnos.eff._
           |
           |""".stripMargin

      h ++ b.mkString("\n")
    }
  }
  object GenAlgebras0 extends Template {
    def file(root: File) = root / "algebras" / "Algebras0.scala"
    def content(tv: TemplateVals): String = {
      import tv._

      val b = nums.map { n =>
        s"""
           |class A${n}s[F[a] <: CopK[_, a]](implicit inj: CopK.Inject[A$n, F]) {
           |  def foo(value: Int): Free[F, Unit] = Free.inject[A$n, F](A$n.Foo(value))
           |}
           |object A${n}s {
           |  implicit def __A${n}s[F[a] <: CopK[_, a]](implicit inj: CopK.Inject[A$n, F]): A${n}s[F] = new A${n}s
           |}
           |
           |class A${n}ss[F[_]](implicit inj: InjectK[A$n, F]) {
           |  def foo(value: Int): Free[F, Unit] = Free.inject[A$n, F](A$n.Foo(value))
           |}
           |object A${n}ss {
           |  implicit def __A${n}ss[F[_]](implicit inj: InjectK[A$n, F]): A${n}ss[F] = new A${n}ss
           |}
           |
           |class A${n}sss[R: A${n}._a${n}] {
           |  def foo(value: Int): Eff[R, Unit] = send(A${n}.Foo(value))
           |}
           |object A${n}sss {
           |  implicit def a${n}sss[R: A${n}._a${n}]: A${n}sss[R] = new A${n}sss[R]
           |}
           |""".stripMargin
      }

      val b2 = nums.map { n =>
        s"""
           |  implicit def a${n}Interpreter[M[_]](implicit M: Monad[M]): A$n ~> M = λ[A$n ~> M] {
           |    case A$n.Foo(_) => M.pure(())
           |  }
           |
           |  def runA${n}[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[A${n}, R, U], reader: types._readerInt[U]): Eff[U, A] = {
           |    translate(effect)(new Translate[A${n}, U] {
           |      def apply[X](kv: A${n}[X]): Eff[U, X] = kv match {
           |        case A${n}.Foo(_) => local[U, Int, X](_ => ())
           |      }
           |    })
           |  }
           |""".stripMargin
      }
      val h =
        s"""package algebras
           |import _root_.cats.{InjectK, Monad, ~>}
           |import _root_.cats.data.Reader
           |import _root_.cats.free.Free
           |import _root_.iota.CopK
           |import _root_.org.atnos.eff._
           |import _root_.org.atnos.eff.all._
           |import _root_.org.atnos.eff.syntax.all._
           |import _root_.org.atnos.eff.interpret._
           |
           |object types {
           |  type _readerInt[R] = Reader[Int, ?] |= R
           |}
           |
           |""".stripMargin

      h ++ b.mkString("\n") ++ b2.mkString("object I {\n", "\n", "}")
    }
  }
  object GenPrograms extends Template {
    def file(root: File) = root / "algebras" / "P.scala"
    def content(tv: TemplateVals): String = {
      import tv._

      @tailrec def genEitherKType(names: List[String], acc: String): String = names match {
        case Nil =>
          acc
        case h :: t if t == Nil =>
          s"EitherK[$h, $acc, A]"
        case h :: t =>
          if (acc.isEmpty)
            genEitherKType(t.tail, s"EitherK[${t.head}, $h, ?]")
          else
            genEitherKType(t, s"EitherK[$h, $acc, ?]")
      }

      val b = nums.map { n =>
        val range = 0.to(n)
        val types = range.map(i => s"A$i").toList.reverse
        val _types = range.map(i => s"A$i._a$i").toList
        s"""object P$n {
           |  type T[A] = CopK[${(range.map(i => s"A$i") :+ "TNilK").mkString(" ::: ")}, A]
           |  def program(implicit ${range.map(i => s"a$i: A${i}s[T]").mkString(", ")}): Free[T, Unit] = for {
           |    ${range.map(i => s"_ <- a$i.foo($i)").mkString("\n")}
           |  } yield ()
           |}
           |
           |object Px$n {
           |  type T[A] = ${if (types.size == 1) "A0[A]" else if (types.size == 2) "EitherK[A0, A1, A]" else genEitherKType(types, "")}
           |  def program(implicit ${range.map(i => s"a$i: A${i}ss[T]").mkString(", ")}): Free[T, Unit] = for {
           |    ${range.map(i => s"_ <- a$i.foo($i)").mkString("\n")}
           |  } yield ()
           |}
           |object Py$n {
           |  def program[R: ${_types.mkString(" : ")} : types._readerInt](implicit ${range.map(i => s"a$i: A${i}sss[R]").mkString(", ")}): Eff[R, Unit] = for {
           |    ${range.map(i => s"_ <- a$i.foo($i)").mkString("\n")}
           |  } yield ()
           |}
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import _root_.cats.data.EitherK
         |import _root_.cats.free.Free
         |import _root_.iota.{CopK, TNilK}
         |import _root_.iota.TListK.:::
         |import _root_.org.atnos.eff._
         |
         |$b
         |""".stripMargin
    }
  }
  object GenRuns extends Template {
    def file(root: File) = root / "algebras" / "Runs.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      @tailrec def genEffInterp(names: List[String], acc: String): String = names match {
        case Nil    => s"${acc}.runReader(0).run"
        case h :: t => genEffInterp(t, s"$h($acc)")
      }

      @tailrec def genEitherKInterp(names: List[String], acc: String): String = names match {
        case Nil =>
          acc
        case h :: t =>
          if (acc.isEmpty)
            genEitherKInterp(t, h)
          else
            genEitherKInterp(t, s"($h or $acc)")
      }

      val b = nums.map { n =>
        val range = 0.to(n)
        s"""object Runs$n {
           |  def run[M[_]](implicit M: Monad[M]): M[Unit] = {
           |    import I._
           |    val fk: P$n.T ~> M = CopK.FunctionK.summon
           |    P$n.program.foldMap(fk)
           |  }
           |  def runC[M[_]](implicit M: Monad[M]): M[Unit] = {
           |    val fk: Px$n.T ~> M = ${genEitherKInterp(range.map(i => s"I.a${i}Interpreter").toList.reverse, "")}
           |    Px$n.program.foldMap(fk)
           |  }
           |  def runE: Unit = {
           |    type Stack = Fx.fx${n + 2}[${range.map(i => s"A${i}").mkString(", ")}, Reader[Int, ?]]
           |    ${genEffInterp(range.map(i => s"I.runA${i}").toList, s"Py$n.program[Stack]")}
           |  }
           |}
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import _root_.cats.{Monad, ~>}
         |import _root_.cats.data.Reader
         |import _root_.cats.free.Free
         |import _root_.iota.CopK
         |import _root_.org.atnos.eff._
         |import _root_.org.atnos.eff.syntax.all._
         |
         |$b
         |""".stripMargin
    }
  }
  object GenIdKBench extends Template {
    def file(root: File) = root / "algebras" / "Benches.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""
           |class Benches${n} extends Base {
           |  @Benchmark
           |  def copk_algebra(b: Blackhole): Unit = b.consume(Runs$n.run[Kleisli[Id, Int, ?]].run(0))
           |  @Benchmark
           |  def eitherK_algebra(b: Blackhole): Unit = b.consume(Runs$n.runC[Kleisli[Id, Int, ?]].run(0))
           |  @Benchmark
           |  def eff_algebra(b: Blackhole): Unit = b.consume(Runs$n.runE)
           |}
         """.stripMargin
      }

      val h =
        s"""package algebras
           |
           |import _root_.cats.Id
           |import _root_.cats.data.Kleisli
           |import _root_.cats.implicits._
           |import _root_.org.openjdk.jmh.annotations._
           |import _root_.org.openjdk.jmh.infra._
           |
           |import scala.concurrent.duration._
           |""".stripMargin

      h + b.mkString("\n")
    }
  }
}

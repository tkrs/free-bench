import sbt._

import scala.annotation.tailrec

object Algebras {

  val templates: Seq[Template] = List(
    GenAlgebra,
    GenAlgebras0,
    GenPrograms,
    GenRuns,
    GenIdKBench
  )

  def gen(dir: File) = for (t <- templates) yield {
    val tgtFile = t.file(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxAlgebras = 11

  class TemplateVals(val num: Int) {
    val nums = 0.until(num)
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
        s"""|sealed trait A$n[A]
            |object A$n {
            |  final case class Foo(value: Int) extends A$n[Unit]
            |  type _a$n[R] = A$n |= R
            |}
            |""".stripMargin
      }
      val h =
        s"""|package algebras
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
        s"""|class A${n}ss[F[_]](implicit inj: InjectK[A$n, F]) {
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
        s"""|
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
        s"""|package algebras
            |import _root_.cats.{InjectK, Monad, ~>}
            |import _root_.cats.data.Reader
            |import _root_.cats.free.Free
            |import _root_.org.atnos.eff._
            |import _root_.org.atnos.eff.all._
            |import _root_.org.atnos.eff.syntax.all._
            |import _root_.org.atnos.eff.interpret._
            |
            |object types {
            |  type _readerInt[R] = Reader[Int, *] |= R
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

      @tailrec def genEitherKType(names: List[String], acc: String): String =
        names match {
          case Nil =>
            acc
          case h :: t if t == Nil =>
            s"EitherK[$h, $acc, A]"
          case h :: t =>
            if (acc.isEmpty)
              genEitherKType(t.tail, s"EitherK[${t.head}, $h, *]")
            else
              genEitherKType(t, s"EitherK[$h, $acc, *]")
        }

      val b = nums.map { n =>
        val range = 0.to(n)
        val types = range.map(i => s"A$i").toList.reverse
        val _types = range.map(i => s"A$i._a$i").toList
        s"""|object Px$n {
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
            |object Pz$n {
            |  def program: Reader[Int, Unit] = for {
            |    ${range.map(i => s"_ <- Reader[Int, Unit](_ => ())").mkString("\n")}
            |  } yield ()
            |}
            |""".stripMargin
      } mkString "\n"

      s"""|package algebras
          |
          |import _root_.cats.data.{EitherK, Reader}
          |import _root_.cats.free.Free
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
      @tailrec def genEffInterp(names: List[String], acc: String): String =
        names match {
          case Nil    => s"${acc}.runReader(0).run"
          case h :: t => genEffInterp(t, s"$h($acc)")
        }

      @tailrec def genEitherKInterp(names: List[String], acc: String): String =
        names match {
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
        s"""|@State(Scope.Benchmark)
            |class Runs$n {
            |  import I._
            |  import instances.M 
            |  private[this] implicit val _m: Monad[M] = M
            |  val fkC: Px$n.T ~> M = ${genEitherKInterp(range.map(i => s"a${i}Interpreter").toList.reverse, "")}
            |  type Stack = Fx.fx${n + 2}[${range.map(i => s"A${i}").mkString(", ")}, M]
            |
            |  def runC: Unit = Px$n.program.foldMap(fkC).run(0)
            |  def runE: Unit = ${genEffInterp(range.map(i => s"runA${i}").toList, s"Py$n.program[Stack]")}
            |  def runR: Unit = Pz$n.program.run(0)
            |}
            |""".stripMargin
      } mkString "\n"

      s"""|package algebras
          |
          |import _root_.cats.{Monad, ~>}
          |import _root_.cats.data.Reader
          |import _root_.cats.free.Free
          |import _root_.org.atnos.eff._
          |import _root_.org.atnos.eff.syntax.all._
          |import _root_.org.openjdk.jmh.annotations._
          |
          |object instances {
          |  import cats.implicits._
          |  type M[A] = Reader[Int, A]
          |  val M = implicitly[Monad[M]]
          |}
          |
          |object states {
          |$b
          |}
          |""".stripMargin
    }
  }
  object GenIdKBench extends Template {
    def file(root: File) = root / "algebras" / "Benches.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""|
            |class Benches${n} extends Base {
            |  @Benchmark
            |  def eitherk_algebra(r: Runs${n}, b: Blackhole): Unit = b.consume(r.runC)
            |  @Benchmark
            |  def eff_algebra(r: Runs${n}, b: Blackhole): Unit = b.consume(r.runE)
            |  @Benchmark
            |  def reader(r: Runs${n}, b: Blackhole): Unit = b.consume(r.runR)
            |}
            |""".stripMargin
      }

      val h =
        s"""|package algebras
            |
            |import _root_.cats.Id
            |import _root_.cats.data.Reader
            |import _root_.org.openjdk.jmh.annotations._
            |import _root_.org.openjdk.jmh.infra._
            |
            |import scala.concurrent.duration._
            |
            |import states._
            |""".stripMargin

      h + b.mkString("\n")
    }
  }
}

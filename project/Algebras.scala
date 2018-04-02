import sbt._

object Algebras {

  val templates: Seq[Template] = List(
    GenAlgebra,
    GenAlgebras0,
    GenPrograms,
    GenRuns,
    GenFutureBench,
    GenFutureKBench
  )

  def gen(dir: File) = for (t <- templates) yield {
    val tgtFile = t.file(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxAlgebras = 50

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
           |trait A$n[A]
           |object A$n {
           |  final case class Foo(value: Int) extends A$n[Unit]
           |}
           |""".stripMargin
      }
      val h =
        s"""package algebras
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
        s"""class A${n}s[F[a] <: CopK[_, a]](implicit inj: CopK.Inject[A$n, F]) {
           |  import A$n._
           |  def foo(value: Int): Free[F, Unit] = Free.inject[A$n, F](Foo(value))
           |}
           |object A${n}s {
           |  implicit def __A${n}s[F[a] <: CopK[_, a]](implicit inj: CopK.Inject[A$n, F]): A${n}s[F] = new A${n}s
           |}
           |object A${n}sC {
           |  import A$n._
           |  implicit def a${n}Interpreter[M[_]](implicit M: MonadError[M, Throwable]): A$n ~> M = λ[A$n ~> M] {
           |    case Foo(_) => M.catchNonFatal(())
           |  }
           |}
           |""".stripMargin
      }
      val h =
        s"""package algebras
           |import cats.{MonadError, ~>}
           |import cats.free.Free
           |import iota.CopK
           |
           |""".stripMargin

      h ++ b.mkString("\n")
    }
  }
  object GenPrograms extends Template {
    def file(root: File) = root / "algebras" / "Programs.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""object Program$n {
           |  type P[A] = CopK[${(0.to(n).map(i => s"A$i") :+ "TNilK").mkString(" ::: ")}, A]
           |  def program(implicit ${0.to(n).map(i => s"a$i: A${i}s[P]").mkString(", ")}): Free[P, Unit] = for {
           |    ${0.to(n).map(i => s"_ <- a$i.foo($i)").mkString("\n")}
           |  } yield ()
           |}
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import cats.free.Free
         |import iota.{CopK, TNilK}
         |import iota.TListK.:::
         |
         |$b
         |""".stripMargin
    }
  }
  object GenRuns extends Template {
    def file(root: File) = root / "algebras" / "Runs.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""object Runs$n {
           |  import ${0.to(n).map(i => s"A${i}sC._").mkString(", ")}
           |  import Program$n._
           |  def run[M[_]](implicit M: MonadError[M, Throwable]): M[Unit] = {
           |    val fk: P ~> M = CopK.FunctionK.summon
           |    program.foldMap(fk)
           |  }
           |}
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import cats.{MonadError, ~>}
         |import cats.free.Free
         |import iota.CopK
         |
         |$b
         |""".stripMargin
    }
  }
  object GenFutureBench extends Template {
    def file(root: File) = root / "algebras" / "FutureBench.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""@Benchmark
           |def algebra$n: Unit = Await.result(Runs$n.run[Future], Duration.Inf)
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import java.util.concurrent.TimeUnit
         |
         |import cats.implicits._
         |import org.openjdk.jmh.annotations._
         |
         |import scala.concurrent.{Await, Future}
         |import scala.concurrent.duration._
         |import scala.concurrent.ExecutionContext.Implicits.global
         |
         |@State(Scope.Benchmark)
         |@BenchmarkMode(Array(Mode.Throughput))
         |@Warmup(iterations = 10, time = 1)
         |@Measurement(iterations = 10, time = 1)
         |@Threads(1)
         |@Fork(2)
         |@OutputTimeUnit(TimeUnit.SECONDS)
         |class FutureBench {
         |
         |$b
         |}
         |""".stripMargin
    }
  }
  object GenFutureKBench extends Template {
    def file(root: File) = root / "algebras" / "FutureKBench.scala"
    def content(tv: TemplateVals): String = {
      import tv._
      val b = nums.map { n =>
        s"""@Benchmark
           |def algebra$n: Unit = Await.result(Runs$n.run[Kleisli[Future, Int, ?]].run(0), Duration.Inf)
           |""".stripMargin
      } mkString "\n"

      s"""package algebras
         |
         |import java.util.concurrent.TimeUnit
         |
         |import cats.data.Kleisli
         |import cats.implicits._
         |import org.openjdk.jmh.annotations._
         |
         |import scala.concurrent.{Await, Future}
         |import scala.concurrent.duration._
         |import scala.concurrent.ExecutionContext.Implicits.global
         |
         |@State(Scope.Benchmark)
         |@BenchmarkMode(Array(Mode.Throughput))
         |@Warmup(iterations = 10, time = 1)
         |@Measurement(iterations = 10, time = 1)
         |@Threads(1)
         |@Fork(2)
         |@OutputTimeUnit(TimeUnit.SECONDS)
         |class FutureKBench {
         |
         |$b
         |}
         |""".stripMargin
    }
  }
}
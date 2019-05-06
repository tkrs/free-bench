package algebras

import _root_.org.openjdk.jmh.annotations._

import scala.concurrent.duration._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 5)
@Threads(1)
@Fork(2)
@OutputTimeUnit(SECONDS)
abstract class Base

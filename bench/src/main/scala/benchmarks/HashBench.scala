package benchmarks

import org.openjdk.jmh.annotations._

import cats.syntax.all.*
import java.util.concurrent.TimeUnit
import chess.format.pgn.{ Fixtures, Reader }
import chess.format.pgn.Reader
import chess.Hash
import chess.Situation
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class HashBench:

  // the unit of CPU work per iteration
  private[this] val Work: Long = 10

  var situations: List[Situation] = _

  @Setup
  def setup() =
    var games = Fixtures.gamesForPerfTest.traverse(Reader.full(_)).toOption.get.traverse(_.valid).toOption.get
    situations = games.flatMap(_.moves).map(_.fold(_.situationAfter, _.situationAfter))

  @Benchmark
  def hashes(bh: Blackhole) =
    var result = situations.map { x =>
      Blackhole.consumeCPU(Work)
      Hash(x)
    }
    bh.consume(result)

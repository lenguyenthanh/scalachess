package benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

import cats.data.Validated
import cats.syntax.option.*

import chess.Pos.*
import chess.variant.Standard
import chess.{ Mode => _, * }
import cats.syntax.all.*
import chess.format.pgn.{ Fixtures, SanStr }
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Measurement(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Warmup(iterations = 15, timeUnit = TimeUnit.SECONDS, time = 3)
@Fork(value = 3)
@Threads(value = 1)
class PlayBench:

  // the unit of CPU work per iteration
  private[this] val Work: Long = 10

  var dividerGames: List[List[Board]] = _
  var gameMoves: List[List[SanStr]]   = _
  var standard: Game                  = _

  def gameReplay(sans: String) =
    Replay.boards(SanStr from sans.split(' ').toList, None, Standard).toOption.get

  @Setup
  def setup() =
    dividerGames = Fixtures.prod500standard.map(gameReplay)

    var nb    = 50
    var games = Fixtures.prod500standard
    gameMoves = games.take(nb).map(g => SanStr from g.split(' ').toList)

    standard = Game(Board init chess.variant.Standard, White)

  @Benchmark
  def divider(bh: Blackhole) =
    var result = dividerGames.map { x =>
      Blackhole.consumeCPU(Work)
      Divider(x)
    }
    bh.consume(result)
    result

  @Benchmark
  def replay(bh: Blackhole) =
    var result = gameMoves.map { moves =>
      Blackhole.consumeCPU(Work)
      Replay.gameMoveWhileValid(moves, chess.format.Fen.initial, chess.variant.Standard)
    }
    bh.consume(result)
    result

  @Benchmark
  def play(bh: Blackhole) =
    var result = standard.playMoves(
      bh,
      E2 -> E4,
      D7 -> D5,
      E4 -> D5,
      D8 -> D5,
      B1 -> C3,
      D5 -> A5,
      D2 -> D4,
      C7 -> C6,
      G1 -> F3,
      C8 -> G4,
      C1 -> F4,
      E7 -> E6,
      H2 -> H3,
      G4 -> F3,
      D1 -> F3,
      F8 -> B4,
      F1 -> E2,
      B8 -> D7,
      A2 -> A3,
      E8 -> C8,
      A3 -> B4,
      A5 -> A1,
      E1 -> D2,
      A1 -> H1,
      F3 -> C6,
      B7 -> C6,
      E2 -> A6
    )
    bh.consume(result)
    result

  extension (game: Game)
    def as(color: Color): Game = game.withPlayer(color)

    def playMoves(bh: Blackhole, moves: (Pos, Pos)*): Validated[String, Game] = playMoveList(bh, moves)

    def playMoveList(bh: Blackhole, moves: Iterable[(Pos, Pos)]): Validated[String, Game] =
      val vg = moves.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, move) =>
        // vg foreach { x =>
        // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        Blackhole.consumeCPU(Work)
        val result = vg map { _.situation.destinations }
        val ng = vg flatMap { g =>
          g(move._1, move._2) map (_._1)
        }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None
    ): Validated[String, Game] =
      game.apply(orig, dest, promotion) map (_._1)

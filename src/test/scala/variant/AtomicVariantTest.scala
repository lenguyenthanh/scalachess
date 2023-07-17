package chess
package variant

import cats.syntax.all.*

import chess.format.EpdFen
import chess.format.pgn.SanStr

class AtomicVariantTest extends ChessTest:

  "Atomic chess" should:

    "Must explode surrounding non pawn pieces on capture" in:
      val fenPosition     = EpdFen("rnbqkbnr/1ppppp1p/p5p1/8/8/1P6/PBPPPPPP/RN1QKBNR w KQkq -")
      val maybeGame       = fenToGame(fenPosition, Atomic)
      val explodedSquares = List(Square.H8, Square.G8)
      val intactPawns     = List(Square.F7, Square.G6, Square.H7)

      val explosionGame = maybeGame flatMap (_.playMoves((Square.B2, Square.H8)))

      explosionGame must beRight.like { case game =>
        explodedSquares.forall(square => game.situation.board(square).isEmpty) must beTrue
        intactPawns.forall(square => game.situation.board(square).isDefined) must beTrue
      }

    "Must explode all surrounding non pawn pieces on capture (contrived situation)" in:
      val fenPosition = EpdFen("k7/3bbn2/3rqn2/3qr3/8/7B/8/1K6 w - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)
      val explodedSquares =
        List(Square.D5, Square.E5, Square.D6, Square.E6, Square.F6, Square.D7, Square.E7, Square.F7)

      val explosionGame = maybeGame.flatMap(_.playMoves((Square.H3, Square.E6)))

      explosionGame must beRight.like { case game =>
        explodedSquares.forall(square => game.situation.board(square).isEmpty) must beTrue
      }

    "Must explode all surrounding non pawn pieces on capture (contrived situation with bottom right position)" in:
      val fenPosition = EpdFen("k7/3bbn2/3rqn2/4rq2/8/1B6/8/K7 w - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)
      val explodedSquares =
        List(Square.F5, Square.E5, Square.D6, Square.E6, Square.F6, Square.D7, Square.E7, Square.F7)

      val explosionGame = maybeGame.flatMap(_.playMoves((Square.B3, Square.E6)))

      explosionGame must beRight.like { case game =>
        explodedSquares.forall(square => game.situation.board(square).isEmpty) must beTrue
      }

    "Not allow a king to capture a piece" in:
      val fenPosition = EpdFen("8/8/8/1k6/8/8/8/1Kr5 w - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val errorGame = maybeGame.flatMap(_.playMoves((Square.B1, Square.C1)))

      errorGame must beLeft("Piece on b1 cannot move to c1")

    "The game must end with the correct winner when a king explodes in the perimeter of a captured piece" in:
      val fenPosition = EpdFen("rnb1kbnr/ppp1pppp/8/3q4/8/7P/PPPP1PP1/RNBQKBNR b KQkq -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val gameWin = maybeGame.flatMap(_.playMoves((Square.D5, Square.D2)))

      gameWin must beRight.like { case winningGame =>
        winningGame.situation.end must beTrue
        winningGame.situation.variantEnd must beTrue
        winningGame.situation.winner must beSome(Black)
      }

    "The game must end by a traditional checkmate (atomic mate)" in:
      val fenPosition = EpdFen("1k6/8/8/8/8/8/PP5r/K7 b - -")
      val maybeGame   = fenToGame(fenPosition, Atomic)

      val gameWin = maybeGame flatMap (_.playMoves((Square.H2, Square.H1)))

      gameWin must beRight.like { case winningGame =>
        winningGame.situation.end must beTrue
        winningGame.situation.variantEnd must beFalse
        winningGame.situation.winner must beSome(Black)
      }

    "Must be a stalemate if a king could usually take a piece, but can't because it would explode" in:
      val positionFen = EpdFen("k7/8/1R6/8/8/8/8/5K2 w - -")
      val maybeGame   = fenToGame(positionFen, Atomic)

      val gameWin = maybeGame flatMap (_.playMoves((Square.B6, Square.B7)))

      gameWin must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.staleMate must beTrue
      }

    "It is stalemate if there are only two kings and two opposite square coloured bishops remaining" in:
      val positionFen = EpdFen("4K3/8/2b5/8/8/8/5B2/3k4 b - -")
      val game        = fenToGame(positionFen, Atomic)

      game must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.autoDraw must beTrue
        game.situation.winner must beNone
        game.situation.status must beSome(Status.Draw)
      }

    "In atomic check, an opportunity at exploding the opponent's king takes priority over getting out of check" in:
      val positionFen    = EpdFen("k1K5/pp5R/8/8/3Q4/P7/1P6/2r5 w - -")
      val threatenedGame = fenToGame(positionFen, Atomic)

      threatenedGame must beRight.like { case (game: Game) =>
        game.situation.check.yes must beTrue
        game.situation.end must beFalse
        game.situation.winner must beNone
        game.situation.moves must haveKeys(Square.D4, Square.H7, Square.C8)
        game.situation.moves.get(Square.D4) must beSome { (moves: List[Move]) =>
          // The queen can defend the king from check
          moves.find(_.dest == Square.A7) must beSome

          // Or explode the opponent's king to win the game
          moves.find(_.dest == Square.C4) must beSome
        }

        // The king cannot capture a piece in the perimeter of the opponent king, exploding itself
        game.situation.moves.get(Square.C8) must beSome:
          (_: List[Move]).forall(_.captures) must beFalse

        // The rook cannot capture, as that would result in our own king exploding
        game.situation.moves.get(Square.H7) must beSome { (mvs: List[Move]) =>
          mvs.find(_.captures) must beNone
          // It can, however, defend the king
          mvs.find(_.dest == Square.C7) must beSome
          mvs.size must beEqualTo(1)
        }
      }

    "In atomic mate, an opportunity at exploding the opponent's king takes priority over getting out of mate" in:
      val positionFen = EpdFen("k1r5/pp5R/8/8/3Q4/8/PP6/K7 b - -")
      val game        = fenToGame(positionFen, Atomic)

      val mateThreatedGame = game.flatMap(_.playMoves((Square.C8, Square.C1)))

      mateThreatedGame must beRight.like { case game =>
        game.situation.end must beFalse
        game.situation.winner must beNone
        game.situation.moves must haveKeys(Square.D4, Square.H7)
        game.situation.legalMoves.forall(_.captures) must beTrue
      }

    "In atomic chess a king may walk into a square that is in the perimeter of the opponent king since it can't capture" in:
      val positionFen = EpdFen("3k4/8/3K4/8/8/8/7r/8 w - -")
      val game        = fenToGame(positionFen, Atomic)

      val successGame = game flatMap (_.playMoves((Square.D6, Square.D7)))

      successGame must beRight.like { case game =>
        game.situation.board(Square.D7) must beSome
        game.situation.check.yes must beFalse
      }

    "Draw on knight and king vs king" in:
      val position = EpdFen("8/1n6/8/8/8/8/k7/2K1b2R w - -")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.H1, Square.E1)))

      successGame must beRight { (game: Game) =>
        game.situation.end must beTrue
        game.situation.status must beSome { (_: Status) == Status.Draw }
      }

    "Draw on bishop and king vs king" in:
      val position = EpdFen("8/1b6/8/8/8/8/k7/2K1n2R w - -")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.H1, Square.E1)))

      successGame must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome { (_: Status) == Status.Draw }
      }

    "Draw on a rook and king vs king" in:
      val position    = EpdFen("8/8/8/8/8/8/N4r2/5k1K b - -")
      val game        = fenToGame(position, Atomic)
      val successGame = game flatMap (_.playMoves((Square.F2, Square.A2)))
      successGame must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome { (_: Status) == Status.Draw }
      }

    "Draw on a king vs a king" in:
      val position    = EpdFen("6r1/8/8/1k6/8/8/2K5/6R1 w - -")
      val game        = fenToGame(position, Atomic)
      val successGame = game flatMap (_.playMoves((Square.G1, Square.G8)))

      successGame must beRight.like { case game =>
        game.situation.end must beTrue
        game.situation.status must beSome { (_: Status) == Status.Draw }
      }

    "It should not be possible to capture a piece resulting in your own king exploding" in:
      val position    = EpdFen("rnbqkbnr/pppNp1pp/5p2/3p4/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 3")
      val game        = fenToGame(position, Atomic)
      val failureGame = game flatMap (_.playMoves((Square.D8, Square.D7)))

      failureGame must beLeft("Piece on d8 cannot move to d7")

    "In an en-passant capture, the pieces surrounding the pawn's destination are exploded along with the pawn" in:
      val position  = EpdFen("4k3/2pppb1p/3r1r2/3P1b2/8/8/1K6/4NB2 b - -")
      val game      = fenToGame(position, Atomic)
      val validGame = game flatMap (_.playMoves((Square.E7, Square.E5), (Square.D5, Square.E6)))

      validGame must beRight.like { case game =>
        game.board(Square.E6) must beNone // The pawn that captures during en-passant should explode
        // Every piece surrounding the en-passant destination square that is not a pawn should be empty
        import bitboard.Bitboard.*
        Square.E6.kingAttacks
          .forall(square =>
            game.board(square).isEmpty || square == Square.E7 || square == Square.D7
          ) must beTrue
      }

    "Verify it is not possible to walk into check" in:
      val position = EpdFen("rnbqkbnr/ppp1pppp/8/3pN3/8/8/PPPPPPPP/RNBQKB1R b KQkq - 1 2")
      val game     = fenToGame(position, Atomic)

      val failureGame = game flatMap (_.playMoves((Square.E8, Square.D7)))

      failureGame must beLeft("Piece on e8 cannot move to d7")

    "Verify that a king can move into what would traditionally be check when touching the opponent king" in:
      val position = EpdFen("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.F6, Square.F5)))

      successGame must beRight

    "After kings have been touching, and one moves away, a king that was protected is under attack again" in:
      val position = EpdFen("r1bq1bnr/pppp1ppp/5k2/4p3/4P1K1/8/PPPP1PPP/RNBQ1B1R b - - 5 6")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.F6, Square.F5), (Square.G4, Square.H3)))

      successGame must beRight.like { game =>
        game.situation.check === Check.Yes
      }

    "Can move into discovered check in order to explode the opponent's king" in:
      val position = EpdFen("R2r2k1/1p2ppbp/8/6p1/2p5/5P1N/P2Pn1PP/2B1K2R b K - 3 19")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.D8, Square.D2)))

      successGame must beRight.like { game =>
        game.situation.end must beTrue
        game.situation.winner must beSome { (_: Color) == Black }
      }

    "It must be possible to remove yourself from check by exploding a piece next to the piece threatening the king" in:
      val position = EpdFen("5k1r/p1ppq1pp/5p2/1B6/1b3P2/2P5/PP4PP/RNB1K2R w KQ - 0 12")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.B5, Square.D7)))

      successGame must beRight.like { game =>
        game.situation.check === Check.No
      }

    "It should not be possible to explode a piece, exploding a piece next to it which would result in a check" in:
      val position = EpdFen("r1b1k2r/pp1pBppp/2p1p2n/q3P3/B2P4/2N2Q2/PPn2PPP/R3K1NR w KQkq - 9 11")
      val game     = fenToGame(position, Atomic)

      val failureGame = game flatMap (_.playMoves((Square.A4, Square.C2)))

      failureGame must beLeft("Piece on a4 cannot move to c2")

    "Game is not a draw when the last piece a player has other than their king is a pawn that is blocked by a mobile piece" in:
      val position = EpdFen("3Q4/2b2k2/5P2/8/8/8/6K1/8 b - - 0 57")
      val game     = fenToGame(position, Atomic)

      val successGame = game flatMap (_.playMoves((Square.C7, Square.D8)))

      successGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    // This would probably be harmless, but there might be a use case where the count of available moves matters,
    // or similar, so best to code defensively.
    "There are no repeated moves in the list of available moves for the situation" in:

      // Situation where the queen can capture a pawn to both win and remove itself from check
      val position    = EpdFen("k1r5/pp5Q/8/8/8/8/PP6/2K5 w - -")
      val successGame = fenToGame(position, Atomic)

      successGame must beRight { (game: Game) =>
        val moves = game.situation.moves.get(Square.H7)

        moves must beSome { (queenMoves: List[Move]) =>
          queenMoves.size must beEqualTo(queenMoves.toSet.size)
        }
      }

    "End move regression" in:
      import Square.*
      "from init" in:
        val game = fenToGame(format.Fen.initial, Atomic)
        val successGame = game flatMap (_.playMoves(
          E2 -> E4,
          D7 -> D5,
          G1 -> F3,
          D5 -> E4,
          F1 -> B5,
          D8 -> D2
        ))
        successGame must beRight.like { case game =>
          game.situation.variantEnd must beTrue
        }
      "from position" in:
        val game = fenToGame(EpdFen("rnbqkbnr/ppp1pppp/8/1B6/8/8/PPPP1PPP/RNBQK2R b KQkq - 1 1"), Atomic)
        val successGame = game flatMap (_.playMoves(D8 -> D2))
        successGame must beRight.like { case game =>
          game.situation.variantEnd must beTrue
        }

    "Not escaping a check that would blow up both kings" in:
      val position = EpdFen("rnbq1bnr/pp1pp1pp/8/2pk1p2/3K1P2/P6P/1PPPP1P1/RNBQ1BNR b - - 0 6")
      val game     = fenToGame(position, Atomic)

      val newGame = game flatMap (_.playMoves(
        Square.A7 -> Square.A6
      ))

      newGame must beRight

    "Identify that a player does not have sufficient material to win when they only have a king" in:
      val position = EpdFen("8/8/8/8/7p/2k4q/2K3P1/8 w - - 19 54")
      val game     = fenToGame(position, Atomic)

      game must beRight.like { case game =>
        game.situation.end must beFalse
      }

      val drawGame = game flatMap (_.playMoves(Square.G2 -> Square.H3))

      drawGame must beRight.like { case game =>
        game.situation.opponentHasInsufficientMaterial must beTrue
      }

    "An automatic draw in a closed position with only kings and pawns which cannot move" in:
      val position     = EpdFen("8/8/6p1/3K4/6P1/2k5/8/8 w - -")
      val originalGame = fenToGame(position, Atomic)

      val game = originalGame flatMap (_.playMoves(Square.G4 -> Square.G5))

      game must beRight.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.end must beTrue
      }

    "Not draw inappropriately on bishops vs bishops (where an explosion taking out the king is possible)" in:
      val position = EpdFen("B2BBBB1/7P/8/8/8/8/3kb3/4K3 w - - 1 53")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.H7,
        Square.H8,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on two bishops (of both square colors)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on bishop and knight" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pK2/5b2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Knight.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on three bishops (of both square colors)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKB1/5B2 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on three bishops (of both square colors)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKB1/6B1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on two bishops and a knight" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKB1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on two bishops and a knight" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKN1/6B1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on two knights and a bishop" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Bishop.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on three knights (of two colors)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKN1/6N1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Knight.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on three knights (of two colors)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKN1/6n1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Knight.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "Not draw inappropriately on three knights (of the same color)" in:
      val position = EpdFen("8/5k2/8/8/8/8/4pKn1/6n1 b - - 1 44")
      val game     = fenToGame(position, Atomic)
      val newGame = game flatMap (_.playMove(
        Square.E2,
        Square.E1,
        Knight.some
      ))

      newGame must beRight.like { case game =>
        game.situation.end must beFalse
      }

    "An automatic draw in a closed position with kings, pawns and a pawnitized bishop" in:
      val position = EpdFen("8/8/2k1p3/5p2/4PP2/1b6/4K3/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.E4, Square.E5))

      newGame must beRight.like { case game =>
        game.situation.autoDraw must beTrue
        game.situation.end must beTrue
      }

    "Not draw inappropriately on blocked pawns with a non-pawnitized bishop" in:
      val position = EpdFen("8/8/2k5/5p2/8/2b2P2/8/3K4 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.F3, Square.F4))

      newGame must beRight.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
      }

    "Not draw inappropriately if both sides have a pawnitized bishop" in:
      val position = EpdFen("6bk/4B2p/8/7P/4K3/8/8/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.H5, Square.H6))

      newGame must beRight.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beFalse
      }

    "Checkmate overrides closed position" in:
      val position = EpdFen("8/8/b1p5/kpP5/p3K3/PP6/8/8 w - - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.B3, Square.B4))

      newGame must beRight.like { case game =>
        game.situation.autoDraw must beFalse
        game.situation.end must beTrue
      }

    "Replay an entire game" in:
      val sans: Vector[SanStr] =
        SanStr from "Nf3 f6 e3 d5 Ng5 fxg5 Qh5+ g6 Qe5 Be6 Bb5+ c6 Qc7 Qxc7 b3 d4 Nc3 dxc3 Bc4 O-O-O O-O h5 Ba3 c5 Bd5 b5 Bb7+ Kb8 c4 h4 d4 Nf6 h3 Ng4 hxg4 h3 g4 h2+ Kh1 Bg7 Rad1 b4 Bb2 Bf5 f3 Bd3 Rxd3 Rh3 d5 Rg3 Be5+ Bxe5 d6 Rg1#"
          .split(' ')
          .toVector
      val (game, steps, error) = chess.Replay.gameMoveWhileValid(sans, Atomic.initialFen, Atomic)
      error must beNone
      steps.size === sans.size

  "castlings" should:

    "Allow castling with touching kings and rook shielding final attack" in:
      val position = EpdFen("8/8/8/8/8/8/4k3/R3K2r w Q - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.E1, Square.C1))

      newGame must beRight.like { case game =>
        game.board(Square.C1) must beEqualTo(White.king.some)
        game.board(Square.D1) must beEqualTo(White.rook.some)
      }

    "Allow castling with touching kings and rook shielding final attack 2" in:
      val position = EpdFen("r3k1rR/5K2/8/8/8/8/8/8 b kq - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame =
        game flatMap (_.playMoves((Square.G8, Square.G6), (Square.F7, Square.E7), (Square.E8, Square.A8)))

      newGame must beRight.like { case game =>
        game.board(Square.C8) must beEqualTo(Black.king.some)
        game.board(Square.D8) must beEqualTo(Black.rook.some)
      }

    "Disallow castling through atomic check" in:
      val position  = EpdFen("8/8/8/8/8/8/5k2/R3K2r w Q - 0 1")
      val game      = fenToGame(position, Atomic)
      val errorGame = game flatMap (_.playMove(Square.E1, Square.C1))
      errorGame must beLeft

    "Disallow castling into atomic check" in:
      val position  = EpdFen("4k3/8/8/8/8/8/8/rR2K3 w Q - 0 1")
      val game      = fenToGame(position, Atomic)
      val errorGame = game flatMap (_.playMove(Square.E1, Square.B1))
      errorGame must beLeft

    "Exploded rooks can't castle" in:
      val position = EpdFen("1r2k3/8/8/8/8/8/1P6/1R2K3 b Q - 0 1")
      val game     = fenToGame(position, Atomic)
      val newGame  = game flatMap (_.playMove(Square.B8, Square.B2))
      newGame must beRight.like { case game =>
        game.situation.legalMoves.filter(_.castles) must beEmpty
      }

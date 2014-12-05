package pgn.parser

import util.parsing.combinator.RegexParsers

object PgnParserMain extends App {
  print(PgnParser.parse(
    "1. e2e4 d7d5 2. e4e5 e7e6 3. d2d4 b7b6 4. Bf1b5+ c7c6 5. Bb5a4 b6b5 \n" +
      "6. Ba4b3 a7a6 7. Ng1e2 f7f6 8. Bc1f4 Nb8d7 9. Nb1d2 h7h6 10. Nd2f3 g7g5 \n" +
      "11. Bf4e3 Bf8b4+ 12. c2c3 Bb4a5 13. e5xf6 Ng8xf6 14. a2a4 O-O 15. a4xb5 a6xb5 \n" +
      "16. Nf3e5 Bc8b7 17. Ne5xd7 Qd8xd7 18. Qd1d3 Nf6g4 19. O-O Ng4xe3 20. Qd3xe3 Bb7a6 \n" +
      "21. Ra1xa5 b5b4 22. c3xb4 g5g4 23. g2g3 Rf8xf2 24. Rf1xf2 Qd7f7 25. Rf2xf7 Kg8xf7 \n" +
      "26. Qe3f4+ Kf7g6 27. Ne2c3 Ba6f1 28. Ra5xa8 Bf1g2 29. Ra8g8+ Kg6h7 30. Qf4f7#"
  ))

  def print(list: List[Step]) {
    list.foreach(println)
  }
}

/**
 * See <a href="http://www6.chessclub.com/help/PGN-spec">PGN-spec on chessclub.com</a>
 * and <a href="http://en.wikipedia.org/wiki/Portable_Game_Notation">Portable_Game_Notation on wikipedia.org</a>.
 *
 * See this site for an online PGN Viewer: http://chesstempo.com/pgn-viewer.html
 *
 * @author János Setény
 * @since 2013.08.27.
 */
object PgnParser extends RegexParsers {

  def stepNumber: Parser[Int] = "\\d+".r <~ "." ^^ {
    case num => num.toInt
  }

  /**
   * <pre>
   * 8.2.3.2: Piece identification
   *
   * SAN identifies each piece by a single upper case letter.  The standard English
   * values: pawn = "P", knight = "N", bishop = "B", rook = "R", queen = "Q", and
   * king = "K".
   * </pre>
   *
   * see <a href="http://www6.chessclub.com/help/PGN-spec">PGN-spec on chessclub.com</a>
   */
  def figure: Parser[String] = "[NBRQK]".r ^^ {
    case "N" => "Knight"
    case "B" => "Bishop"
    case "R" => "Rook"
    case "Q" => "Queen"
    case "K" => "King"
  }

  def field: Parser[String] = "[a-h][1-8]".r

  def checkingMove: Parser[Move] = "+" ^^ {
    case _ => new CheckingMove
  }

  def checkMatingMove: Parser[Move] = "#" ^^ {
    case _ => new CheckMatingMove
  }

  def normalHalfStep: Parser[HalfStep] =
    opt(figure) ~ field ~ opt("x")  ~ field ~ opt(checkingMove | checkMatingMove) ^^ {
      case Some(fig) ~ from  ~ capturing ~ to    ~ moveType
      => NormalHalfStep(fig, from, to, moveType, capturing)

      case None         ~ from  ~ capturing ~ to    ~ moveType
      => NormalHalfStep("Pawn", from, to, moveType, capturing)
    }

  def kingSideCastling: Parser[HalfStep] = "O-O" ^^ {
    case _ => new KingSideCastling
  }

  def queenSideCastling: Parser[HalfStep] = "O-O-O" ^^ {
    case _ => new QueenSideCastling
  }

  def halfStep = normalHalfStep | kingSideCastling | queenSideCastling

  def oneStep: Parser[Step] = stepNumber ~ halfStep ~ opt(halfStep) ^^ {
    case n ~ white ~ black => Step(n, white, black)
  }

  def steps: Parser[List[Step]] = oneStep *

  def parse(input: String): List[Step] =
    parseAll(steps, input) match {
      case Success(expression, _) => expression
      case f: NoSuccess => throw new IllegalArgumentException(f.msg)
    }
}

case class Step(num: Int, white: HalfStep, black: Option[HalfStep])

abstract class HalfStep

case class NormalHalfStep(figure: String, from: String, to: String,
                          moveType: Option[Move], capturing: Option[String]) extends HalfStep {

  override def toString = s"$figure: $from, $to" + moveAsString

  private def moveAsString: String = (moveType, capturing) match {
    case (None, None) => ""
    case (None, Some(_)) => ", capture"
    case (Some(_: CheckingMove), None) => ", check"
    case (Some(_: CheckMatingMove), None) => ", check mate"
    case (Some(_: CheckingMove), Some(_)) => ", capture and check"
    case (Some(_: CheckMatingMove), Some(_)) => ", capture and check mate"
  }
}

/** "O-O" */
class KingSideCastling extends HalfStep

/** "O-O-O" */
class QueenSideCastling extends HalfStep


abstract class Move

//object NormalMove extends Move
class CheckingMove extends Move

class CheckMatingMove extends Move

abstract class Figure
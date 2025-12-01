package apps.stratego

import cs214.webapp.WireFormat
import upickle.default.*
import scala.util.Try


/*=== Helper function===
  to serialize and deserialize coordinates */
object CoordFormat extends WireFormat[Coord]:
  override def encode(c: Coord): Value =
    Obj("row" -> c.row, "col" -> c.col)

  override def decode(js: Value): Try[Coord] = Try:
    Coord(js("row").num.toInt, js("col").num.toInt)


/*=== Helper function=== 
  to serialize and deserialize Troops */
object TroopFormat extends WireFormat[Troop]:
  override def encode(t: Troop): Value =
    Obj(
      "name" -> t.name,
      "rank" -> t.rank,
      "owner" -> t.owner,
      "revealed" -> t.revealed
    )

  override def decode(js: Value): Try[Troop] = Try:
    Troop(
      name = js("name").str,
      rank = js("rank").num.toInt,
      owner = js("owner").str,
      revealed = js("revealed").bool
    )
//===Event firnat serialize and deserialize===

object eventFormat extends WireFormat[Event]:
  import Event.*
  /*Almost identical to the memory example i 
  just added helper functions for the extra*/
  override def encode(e: Event): Value = e match
    case SquareClicked(coord) =>
      Obj("type" -> "SquareClicked", "coord" -> CoordFormat.encode(coord))
    case KeyPressed(key) =>
      Obj("type" -> "KeyPressed", "key" -> key)

  override def decode(js: Value): Try[Event] = Try:
    js("type").str match
      case "SquareClicked" =>
        SquareClicked(CoordFormat.decode(js("coord")).get)
      case "KeyPressed" =>
        KeyPressed(js("key").str)
      case _ =>
        throw DecodingException(s"Invalid Stratego event: $js")


// ==== View Format ====
/*for now our implementation only has state doesnt 
have matched like memmory or anything else so its a simpler version*/
object viewFormat extends WireFormat[View]:
  override def encode(v: View): Value =
    Obj("state" -> stateViewFormat.encode(v.state))

  override def decode(js: Value): Try[View] = Try:
    View(stateViewFormat.decode(js("state")).get)
  

//==== StateView Format ====
/*again its pretty simillar to the memmory example we
 just have different enums */
object stateViewFormat extends WireFormat[StateView]:
  import StateView.*
  /*board of the game */
  val troopVectorWire = VectorWire(troopViewFormat)
  val winnerIdsWire = SetWire(StringWire)

  override def encode(view: StateView): Value = view match
    case Placing(phase, board) =>
      Obj(
        "type" -> "Placing",
        "phase" -> phaseViewFormat.encode(phase),
        "board" -> troopVectorWire.encode(board)
      )
    case Playing(phase, currentPlayer, board) =>
      Obj(
        "type" -> "Playing",
        "phase" -> phaseViewFormat.encode(phase),
        "currentPlayer" -> currentPlayer,
        "board" -> troopVectorWire.encode(board)
      )
    case Finished(winnerIds) =>
      Obj(
        "type" -> "Finished",
        "winnerIds" -> winnerIdsWire.encode(winnerIds)
      )

  override def decode(js: Value): Try[StateView] = Try:
    js("type").str match
      case "Placing" =>
        Placing(
          phase = phaseViewFormat.decode(js("phase")).get,
          board = troopVectorWire.decode(js("board")).get.to(Vector)
        )
      case "Playing" =>
        Playing(
          phase = phaseViewFormat.decode(js("phase")).get,
          currentPlayer = js("currentPlayer").str,
          board = troopVectorWire.decode(js("board")).get.to(Vector)
        )
      case "Finished" =>
        Finished(winnerIdsWire.decode(js("winnerIds")).get)
      case _ =>
        throw DecodingException(s"Unexpected state view: $js")



// ==== PhaseView wire format =====
/*againg identical to memmory game 
  i want to ask a ta why the dont encode,decode each part of the enum 
  and just turn it to string on memory game (maybe because it 
  doesnt have parameters and they are all just names*/
object phaseViewFormat extends WireFormat[PhaseView]:
  import PhaseView.*

  override def encode(p: PhaseView): Value =
    Str(p.toString)

  override def decode(js: Value): Try[PhaseView] = Try:
    try PhaseView.valueOf(js.str)
    catch
      case _: IllegalArgumentException =>
        throw DecodingException(s"Unexpected phase view: $js")

// ====TroopView wire format ====
/*serialize and deserialize the troops again same as cardView of the example
  just different names and enums */

object troopViewFormat extends WireFormat[TroopView]:
  import TroopView.*

  override def encode(v: TroopView): Value = v match
    case Covered =>
      Obj("type" -> "Covered")
    case Uncovered(troop) =>
      Obj("type" -> "Uncovered", "troop" -> TroopFormat.encode(troop))
    case DeadView(troop) =>
      Obj("type" -> "DeadView", "troop" -> TroopFormat.encode(troop))

  override def decode(js: Value): Try[TroopView] = Try:
    js("type").str match
      case "Covered" => Covered
      case "Uncovered" => Uncovered(TroopFormat.decode(js("troop")).get)
      case "DeadView" => DeadView(TroopFormat.decode(js("troop")).get)
      case _ => throw DecodingException(s"Unexpected troop view: $js")


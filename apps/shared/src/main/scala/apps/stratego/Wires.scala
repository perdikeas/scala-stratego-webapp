package apps.ul2025app100

import cs214.webapp.*
import scala.util.Try
import ujson.*

/**
  * Wire encoders/decoders for Stratego.
  *
  * These types are shared between server and client and tell the framework
  * how to (de)serialize:
  *   - Coord
  *   - Troop
  *   - Event
  *   - View / StateView / SquareView / TroopView
  */
object Wire extends AppWire[Event, View]:

  // ------------------------------------------------------------
  // COORD FORMAT
  // ------------------------------------------------------------

  /** JSON encoding for board coordinates. */
  object CoordFormat extends WireFormat[Coord]:
    override def encode(c: Coord): Value =
      Obj("row" -> c.row, "col" -> c.col)

    override def decode(js: Value): Try[Coord] = Try:
      Coord(js("row").num.toInt, js("col").num.toInt)

  // ------------------------------------------------------------
  // TROOP FORMAT
  // ------------------------------------------------------------

  /** JSON encoding for Troop pieces. */
  object TroopFormat extends WireFormat[Troop]:
    override def encode(t: Troop): Value =
      Obj(
        "name"     -> t.name,
        "rank"     -> t.rank,
        "owner"    -> t.owner,
        "revealed" -> t.revealed
      )

    override def decode(js: Value): Try[Troop] = Try:
      Troop(
        name     = js("name").str,
        rank     = js("rank").num.toInt,
        owner    = js("owner").str,
        revealed = js("revealed").bool
      )

  // ------------------------------------------------------------
  // EVENT FORMAT
  // ------------------------------------------------------------

  /** JSON encoding for events sent from clients to server. */
  object eventFormat extends WireFormat[Event]:
    override def encode(e: Event): Value = e match
      case Event.SquareClicked(coord) =>
        Obj("type" -> "SquareClicked", "coord" -> CoordFormat.encode(coord))
      case Event.KeyPressed(key) =>
        Obj("type" -> "KeyPressed", "key" -> key)

    override def decode(js: Value): Try[Event] = Try:
      js("type").str match
        case "SquareClicked" =>
          Event.SquareClicked(CoordFormat.decode(js("coord")).get)
        case "KeyPressed" =>
          Event.KeyPressed(js("key").str)
        case _ =>
          throw DecodingException(s"Invalid Stratego event: $js")

  // ------------------------------------------------------------
  // VIEW FORMAT (top-level View wrapper)
  // ------------------------------------------------------------

  object viewFormat extends WireFormat[View]:
    override def encode(v: View): Value =
      Obj("state" -> stateViewFormat.encode(v.state))

    override def decode(js: Value): Try[View] = Try:
      View(stateViewFormat.decode(js("state")).get)

  // ------------------------------------------------------------
  // STATE VIEW FORMAT
  // ------------------------------------------------------------

  /** Encoding for the state viewed by clients (playing, placing, finished). */
  object stateViewFormat extends WireFormat[StateView]:

    private val squareVectorWire = VectorWire(squareViewFormat)
    private val winnerIdsWire    = SetWire(StringWire)
    private val coordSetWire     = SetWire(CoordFormat)
    private val troopWire        = TroopFormat // for nextTroop field

    override def encode(view: StateView): Value = view match
      case StateView.Placing(phase, board,troop) =>
        Obj(
          "type"  -> "Placing",
          "phase" -> phaseViewFormat.encode(phase),
          "board" -> squareVectorWire.encode(board),
          "nextTroop" -> troop.map(troopWire.encode).getOrElse(Null)
        )

      case StateView.Playing(phase, currentPlayer, board, selected, highlights) =>
        Obj(
          "type"          -> "Playing",
          "phase"         -> phaseViewFormat.encode(phase),
          "currentPlayer" -> currentPlayer,
          "board"         -> squareVectorWire.encode(board),
          "selected"      -> selected.map(CoordFormat.encode).getOrElse(Null),
          "highlights"    -> coordSetWire.encode(highlights)
        )

      case StateView.Finished(winnerIds) =>
        Obj(
          "type"      -> "Finished",
          "winnerIds" -> winnerIdsWire.encode(winnerIds)
        )

    override def decode(js: Value): Try[StateView] = Try:
      js("type").str match
        case "Placing" =>
          StateView.Placing(
            phase = phaseViewFormat.decode(js("phase")).get,
            board = squareVectorWire.decode(js("board")).get.to(Vector),
            nextTroop =
              if js.obj.contains("nextTroop") && js("nextTroop") != Null then
                Some(troopWire.decode(js("nextTroop")).get)
              else None
          )

        case "Playing" =>
          val selectedVal = js("selected")
          val selectedOpt =
            if selectedVal == Null then None
            else Some(CoordFormat.decode(selectedVal).get)

          StateView.Playing(
            phase         = phaseViewFormat.decode(js("phase")).get,
            currentPlayer = js("currentPlayer").str,
            board         = squareVectorWire.decode(js("board")).get.to(Vector),
            selected      = selectedOpt,
            highlights    = coordSetWire.decode(js("highlights")).get
          )

        case "Finished" =>
          StateView.Finished(
            winnerIdsWire.decode(js("winnerIds")).get
          )

        case other =>
          throw DecodingException(s"Unexpected state view: $other")

  // ------------------------------------------------------------
  // PHASE VIEW FORMAT
  // ------------------------------------------------------------

  /** Encode PhaseView using its string name (like in the memory example). */
  object phaseViewFormat extends WireFormat[PhaseView]:
    override def encode(p: PhaseView): Value =
      Str(p.toString)

    override def decode(js: Value): Try[PhaseView] = Try:
      try PhaseView.valueOf(js.str)
      catch
        case _: IllegalArgumentException =>
          throw DecodingException(s"Unexpected phase view: $js")

  // ------------------------------------------------------------
  // TROOP VIEW FORMAT
  // ------------------------------------------------------------

  /** Encode how a troop appears on the client (covered / uncovered / dead). */
  object troopViewFormat extends WireFormat[TroopView]:
    override def encode(tv: TroopView): Value = tv match
      case TroopView.Covered =>
        Obj("type" -> "Covered")
      case TroopView.Uncovered(troop) =>
        Obj("type" -> "Uncovered", "troop" -> TroopFormat.encode(troop))
      case TroopView.DeadView(troop) =>
        Obj("type" -> "DeadView", "troop" -> TroopFormat.encode(troop))

    override def decode(js: Value): Try[TroopView] = Try:
      js("type").str match
        case "Covered" =>
          TroopView.Covered
        case "Uncovered" =>
          TroopView.Uncovered(TroopFormat.decode(js("troop")).get)
        case "DeadView" =>
          TroopView.DeadView(TroopFormat.decode(js("troop")).get)
        case _ =>
          throw DecodingException(s"Unexpected troop view: $js")

  // ------------------------------------------------------------
  // SQUARE VIEW FORMAT
  // ------------------------------------------------------------

  /** Encode what is rendered at a particular board coordinate. */
  object squareViewFormat extends WireFormat[SquareView]:
    override def encode(v: SquareView): Value = v match
      case SquareView.Empty(coord) =>
        Obj(
          "type"  -> "Empty",
          "coord" -> CoordFormat.encode(coord)
        )

      case SquareView.HasTroop(coord, troopView) =>
        Obj(
          "type"  -> "HasTroop",
          "coord" -> CoordFormat.encode(coord),
          "troop" -> troopViewFormat.encode(troopView)
        )

    override def decode(js: Value): Try[SquareView] = Try:
      js("type").str match
        case "Empty" =>
          SquareView.Empty(CoordFormat.decode(js("coord")).get)

        case "HasTroop" =>
          SquareView.HasTroop(
            CoordFormat.decode(js("coord")).get,
            troopViewFormat.decode(js("troop")).get
          )

        case other =>
          throw DecodingException(s"Unexpected square view: $other")

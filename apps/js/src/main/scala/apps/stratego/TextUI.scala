package apps.ul2025app100

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.{HTMLAttribute, TextClientAppInstance, MouseEvent}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel

// Import phase / square enums and lake coordinates
import PhaseView.*
import SquareView.*
import BoardConstants.*

/** Entry point for the text-based Stratego UI (loaded from index.html). */
@JSExportTopLevel("stratego_text")
object TextUI extends WSClientApp:
  /** App id must match the server app id (Logic.appInfo.id). */
  def appId: String = "ul2025app100"
  /** UI id to distinguish between multiple frontends (e.g., text vs graphics). */
  def uiId: String  = "text"

  /** Create one client instance per browser tab / user. */
  def init(
      userId: UserId,
      sendMessage: ujson.Value => Unit,
      target: dom.Element
  ): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)

/** Single client instance: handles rendering and user input. */
class TextUIInstance(
    userId: UserId,
    sendMessage: ujson.Value => Unit,
    target: dom.Element
) extends TextClientAppInstance[Event, View](userId, sendMessage, target):

  /** Same wire as on the server side. */
  override val wire = Wire

  /** We don't use text commands in this UI, only clicks. */
  override def handleTextInput(view: View, text: String): Option[Event] =
    None

  import cs214.webapp.client.graphics.{TextSegment as TS}

  /** Top-level render: header + game view. */
  override def renderView(userId: UserId, view: View): Vector[TS] =
    renderHeader() ++ renderInternal(view)

  // ---------------------------------------------------------------------------
  // Header + sub-headers
  // ---------------------------------------------------------------------------

  /** Static title shown above the board. */
  private def renderHeader(): Vector[TS] =
    Vector(
      TS(
        text = "Stratego: ",
        cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")
      ),
      TS(
        "Capture your opponent's flag!\n\n",
        cssProperties = Map("font-style" -> "italic", "font-size" -> "110%")
      )
    )

  /** Decide what to show depending on the server-side state. */
  private def renderInternal(view: View): Vector[TS] =
    view.state match
      case StateView.Placing(phase, board,troop) =>
        renderSubHeaderPlacing(phase,troop) ++ renderBoard(board, None, Set.empty)

      case StateView.Playing(phase, currentPlayer, board, selected, highlights) =>
        renderSubHeaderPlaying(phase, currentPlayer) ++
          renderBoard(board, selected, highlights)
          //renderDeadTroops(view)
      case StateView.Finished(winnerIds) =>
        renderFinished(winnerIds)
  
  
  /** Short text explaining what's happening during the placing phase. */
  private def renderSubHeaderPlacing(phase: PhaseView, troop: Option[Troop]): Vector[TS] =
    val text =
      phase match
        case ProperPlacement =>
          "Place your troops on your side of the board.\n\n"
        case WaitingPlacing =>
          "Waiting for the other player to finish placing troops.\n\n"
        case _ =>
          "\n"
    val troopText = troop match 
      case Some(t1) =>
        val iconStr = t1.name match
                  case "Bomb" => "💣"
                  case "Flag" => "🚩"
                  case "Marshal" => "M"
                  case _      => t1.rank.toString
        s"Next troop to place: $iconStr \n\n"
      case _ => "No troops left to place \n\n"
    Vector(TS(text+troopText))

  /** Short text explaining whose turn it is and what they can do. */
  private def renderSubHeaderPlaying(phase: PhaseView, currentPlayer: UserId): Vector[TS] =
    val text =
      phase match
        case Selecting if currentPlayer == userId =>
          "It's your turn: select one of your movable troops.\n\n"
        case Attacking if currentPlayer == userId =>
          "Choose a destination (move) or enemy to attack.\n\n"
        case WaitingAttacking =>
          s"It's $currentPlayer's turn.\n\n"
        case Selecting =>
          s"$currentPlayer is selecting a troop.\n\n"
        case Attacking =>
          s"$currentPlayer is choosing where to move or attack.\n\n"
        case _ =>
          s"Turn of $currentPlayer.\n\n"
    Vector(TS(text))

  /** Message shown at the end of the game. */
  private def renderFinished(winnerIds: Set[UserId]): Vector[TS] =
    Vector(
      TS(
        text = s"Game over! Winner(s): ${winnerIds.toSeq.sorted.mkString(", ")}\n\n",
        cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")
      )
    )

  // ---------------------------------------------------------------------------
  // Board rendering
  // ---------------------------------------------------------------------------

  /** Render the full 10x10 board, with selection + highlight info. */
  private def renderBoard(
      board: Seq[SquareView],
      selected: Option[Coord],
      highlights: Set[Coord]
  ): Vector[TS] =
    val clickAt = onClick()
    board.zipWithIndex
      .map: (sq, idx) =>
        val coord = idxToCoord(idx)
        renderSquare(
          sq,
          coord,
          isSelected   = selected.contains(coord),
          isHighlighted = highlights.contains(coord),
          onClick      = clickAt(idx)
        )
      .toVector
      .grouped(10)                     // 10 squares per row
      .flatMap(row => row :+ TS("\n")) // newline after each row
      .toVector :+ TS("\n\n")

  /** Map a linear index (0..99) to a (row, col) coordinate. */
  private def idxToCoord(idx: Int): Coord =
    Coord(row = idx / 10, col = idx % 10)

  /** Always allow clicking on squares; server will validate moves. */
  private def onClick()(idx: Int) =
    Some(() => sendEvent(Event.SquareClicked(idxToCoord(idx))))

  /** Render one square: icon + styling + click handler. */
  private def renderSquare(
      square: SquareView,
      coord: Coord,
      isSelected: Boolean,
      isHighlighted: Boolean,
      onClick: Option[() => Unit]
  ): TS =
    // Base icon and color depending on the square contents
    val (icon, baseStyle): (String, Map[String, String]) =
      square match
        case HasTroop(_, troopView) =>
          troopView match
            case TroopView.Covered =>
              // Hidden enemy troop (or our own in fog of war) = red box
              ("🟥", Map.empty[String, String])

            case TroopView.Uncovered(troop) =>
              // Show rank or special emoji, and color by owner
              val iconStr =
                troop.name match
                  case "Bomb" => "💣"
                  case "Flag" => "🚩"
                  case "Marshal" => "M"
                  case _      => troop.rank.toString

              val color =
                if troop.owner == userId then "#b71c1c"   // our pieces = red
                else "#0d47a1"                            // opponent  = blue

              (iconStr, Map("color" -> color, "font-weight" -> "bold"))

            case TroopView.DeadView(_) =>
              // Dead pieces: faint cross
              ("✖", Map("opacity" -> "0.4"))

        case Empty(c) =>
          // Empty tiles: lakes vs normal ground
          if lakes.contains(c) then
            ("🟦", Map.empty[String, String]) // lakes
          else
            ("⬜", Map.empty[String, String]) // normal empty square

    // Extra styling for selection + legal destinations
    val selectionStyle: Map[String, String] =
      if isSelected then
        Map(
          "border" -> "2px solid #ffeb3b",
          "box-shadow" -> "0 0 4px #ffeb3b"
        )
      else Map.empty

    val highlightStyle: Map[String, String] =
      if isHighlighted then
        Map("background-color" -> "rgba(255, 255, 0, 0.2)")
      else Map.empty

    // Translate optional click handler into a MouseEvent handler
    val onMouseEvent =
      onClick.map(handler =>
        (evt: MouseEvent) =>
          evt match
            case MouseEvent.Click(_) => handler()
            case _                   => ()
      )

    // Final CSS properties for this square
    val squareStyle: Map[String, String] =
      Map(
        "font-family" -> "var(--emoji)",
        "line-height" -> "1.3",
        "font-size"   -> "180%"
      ) ++ baseStyle
        ++ selectionStyle
        ++ highlightStyle
        ++ (if onMouseEvent.nonEmpty then Map("cursor" -> "pointer") else Map.empty)

    TS(
      text = icon,
      cssProperties = squareStyle,
      onMouseEvent = onMouseEvent
    )

  // ---------------------------------------------------------------------------
  // Extra CSS for the whole UI
  // ---------------------------------------------------------------------------

  /** Light background + allow long lines to wrap nicely. */
  override def css: String = super.css +
    """
      |html {
      |  background: #FAFAFA;
      |}
      |pre {
      |  word-break: break-all;
      |}
      |""".stripMargin


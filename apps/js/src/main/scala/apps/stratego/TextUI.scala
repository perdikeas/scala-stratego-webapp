package apps
package stratego

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.{HTMLAttribute, TextClientAppInstance, MouseEvent}

import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel
import PhaseView.*

// start TextUI
@JSExportTopLevel("stratego_text")
object TextUI extends WSClientApp:
  def appId: String = "stratego"
  def uiId: String = "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)
// end TextUI

// start TextUIInstanceClass
class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element)
    extends TextClientAppInstance[Event, View](userId, sendMessage, target):
// end TextUIInstanceClass

  override val wire = stratego.Wire
  // start wire
  // end wire

  // start handleTextInput
  override def handleTextInput(view: View, text: String): Option[Event] =
    None
  // end handleTextInput

  // start renderView
  import cs214.webapp.client.graphics.{TextSegment as TS}
  override def renderView(userId: UserId, view: View): Vector[TS] =
    renderHeader() ++ renderInternal(view)
  // end renderView

  // start renderHeader
  private def renderHeader(): Vector[TS] =
    Vector(
      TS(text = "Stratego: ", cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")),
//      TS("Pick pairs of matching cards!", cssProperties = Map("font-style" -> "italic", "font-size" -> "120%")),
      TS("\n\n")
    )
  // end renderHeader

  private def renderInternal(view: View): Vector[TS] = 
    view.state match
      case StateView.Placing(phase, board) => 
        renderBoard(board, true, true)        
      case StateView.Playing(phase, currentPlayer, board) => println("Playing")
        renderBoard(board, true, true) 
      case StateView.Finished(winnerIds) => Vector(
              TS(text = "Congrats: ", cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")),
              TS("\n\n")
              )
  private def renderBoard(board: Vector[TroopView], enabled: Boolean, allowClick: Boolean): Vector[TS] =
    val oC = onClick(allowClick)
    board.zipWithIndex.map: (c, idx) =>
      renderTroop(c, enabled, Map("font-size" -> "160%"), oC(idx))
    .toVector.grouped(10)                       // split into rows of 10
    .flatMap(row => row :+ TS("\n"))   // add newline after each row
    .toVector
      :+ TS("\n\n")

  private def onClick(allowClick: Boolean)(idx: Int) =
    if allowClick then Some(() => sendEvent(Event.SquareClicked(Coord(idx % 10, idx / 10))))
    else None

  private def renderTroop(
      troop: TroopView,
      enabled: Boolean,
      style: Map[String, String],
      onClick: Option[() => Unit]
  ): TS =
    val troopIcon = troop match
      case TroopView.Covered => "🟥"
      case TroopView.Uncovered(troop) => 
        if (troop.name == "Bomb") then "💣"
        else if (troop.name == "Flag") then "🚩"
        else troop.rank.toString
      case TroopView.DeadView(troop) => "x"
    val onMouseEvent = onClick
//      .filter(_ => !alreadyMatched)
      .map(handler =>
        (evt: MouseEvent) =>
          evt match
            case MouseEvent.Click(_) => handler()
            case _                   => ()
      )      
    val troopStyle =
      Map("font-family" -> "var(--emoji)", "line-height" -> "1.3")
        ++ (if onMouseEvent.nonEmpty then Map("cursor" -> "pointer") else Map())
//        ++ (if alreadyMatched || !enabled then Map("opacity" -> "0.7") else Map())
        ++ style
    TS(troopIcon, cssProperties = troopStyle, onMouseEvent = onMouseEvent)

  // start css
  override def css: String = super.css +
    """
    html {
      background: #FAFAFA;
    }
    pre {
      word-break: break-all;
    }
    """
  // end css
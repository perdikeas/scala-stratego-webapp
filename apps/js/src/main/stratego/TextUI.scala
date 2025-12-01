package apps
package stratego
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("stratego_text")
object TextUI extends WSClientApp:
    def appId: String = "stratego"
    def uiId: String = "text"

    def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target):
    
    override val wire = stratego.Wire
    /*val pieceSelected = Map(
    "Left" -> key.Left,
    "l" -> key.Left,
    "Right" -> key.Right,
    "r" -> key.Right,
    "Up" -> key.Up,
    "u" -> key.Up,
    "Down" -> key.Down,
    "d" -> Key.Down
  )*/
    override def handleTextInput(view: View, text: String): Option[Event] =
        def isOnlyLetters(s: String): Boolean =
             s.forall(ch => Character.isLetter(ch))
        def coord(s: String): (Int, Int) =
            val parts = s.split(",")
            (parts(0).toInt, parts(1).toInt)
        
        if (isOnlyLetters(text.toLowerCase())) then
            //pieceSelected.get(text.toLowerCase())
            //      .map(Event.KeyPressed.apply)
            Event.KeyPressed(text.toLowerCase)
        else
            Event.SquareClicked(coord(text.toLowerCase()))
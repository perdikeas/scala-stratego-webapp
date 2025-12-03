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
    override def handleTextInput(view: View, text: String): Option[Event] =
        None
    import cs214.webapp.client.graphics.{TextSegment as TS}
    override def renderView(userId: UserId, view: View): Vector[TS] =
        renderHeader() ++ renderInternal(view)
    // end renderView

    // start renderHeader
    private def renderHeader(): Vector[TS] =
        Vector(
        TS(text = "Stratego: ", cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")),
        TS("Pick pairs of matching cards!", cssProperties = Map("font-style" -> "italic", "font-size" -> "120%")),
        TS("\n\n")
        )
    // end renderHeader

    private def renderInternal(view: View): Vector[TS] = 
        Vector(
        TS(text = "Hello: ", cssProperties = Map("font-weight" -> "bold", "font-size" -> "120%")),
        TS("\n\n")
        )

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
package apps
package stratego
import cs214.webapp.UserId

import scala.util.{Random, Try}
import cs214.webapp.*
import cs214.webapp.server.{StateMachine}

class Logic extends StateMachine[Event, State, View]:
  val appInfo: AppInfo = AppInfo(
    id = "stratego",
    name = "Stratego",
    description = " is trategic game.",
    year = 2025
  )
  // start wire
  override val wire = stratego.Wire
  // end wire

  override def init(clients: Seq[UserId]): State = 
      State( board = Vector.empty[Square],              // or Vector.fill(100)(Square.empty)
  selectedSquare = None,
  selectedTroop = None,
  dead = Map.empty[UserId, Set[Troop]],
  players = Vector.empty[UserId],
  inCombat = Map.empty[UserId, Troop],
  phase = Phase.PlacingTroops,
  currentPlayer = "" // or some default UserId

    )

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
    import Action.*
    Seq(Pause(0))

    // end transition

    // start project
  override def project(state: State)(userId: UserId): View =
    View(StateView.Finished(Set.empty))
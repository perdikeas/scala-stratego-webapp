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

  def initB(clients: Seq[UserId]): Map[Coord, Troop] =
    val players: Vector[UserId] = clients.toVector
    val initTroops = GameLogic.initTroops(players(0), players(1))    
    Map(Coord(9,9)->initTroops(0), 
      Coord(9,8) -> initTroops(1), 
      Coord(9,7) -> initTroops(2), 
      Coord(9,6) -> initTroops(3),
      Coord(9,5) -> initTroops(4),
      Coord(9,4) -> initTroops(5),
      Coord(9,3) -> initTroops(6),
      Coord(9,2) -> initTroops(7),
      Coord(9,1) -> initTroops(8),
      Coord(9,0) -> initTroops(9),
      Coord(8,9) -> initTroops(10),
      Coord(8,8) -> initTroops(11),
      Coord(8,7) -> initTroops(12),
      Coord(8,6) -> initTroops(13),
      Coord(8,5) -> initTroops(14),
      Coord(8,4) -> initTroops(15),
      Coord(8,3) -> initTroops(16),
      Coord(8,2) -> initTroops(17),
      Coord(8,1) -> initTroops(18),
      Coord(8,0) -> initTroops(19),
      //player2      
      Coord(0,9) -> initTroops(20),
      Coord(0,8) -> initTroops(21),
      Coord(0,7) -> initTroops(22),
      Coord(0,6) -> initTroops(23),
      Coord(0,5) -> initTroops(24),
      Coord(0,4) -> initTroops(25),
      Coord(0,3) -> initTroops(26),
      Coord(0,2) -> initTroops(27),
      Coord(0,1) -> initTroops(28),
      Coord(0,0) -> initTroops(29),
      Coord(1,9) -> initTroops(30),
      Coord(1,8) -> initTroops(31),
      Coord(1,7) -> initTroops(32),
      Coord(1,6) -> initTroops(33),
      Coord(1,5) -> initTroops(34),
      Coord(1,4) -> initTroops(35),
      Coord(1,3) -> initTroops(36),
      Coord(1,2) -> initTroops(37),
      Coord(1,1) -> initTroops(38),
      Coord(1,0) -> initTroops(39)
    )

  override def init(clients: Seq[UserId]): State = 
    println("init:")
    val state = State(board = GameLogic.emptyBoard(),              // or Vector.fill(100)(Square.empty)
      selectedSquare = None,
      selectedTroop = None,
      dead = Map.empty[UserId, Set[Troop]],
      players = Vector.empty[UserId],
      inCombat = Map.empty[UserId, Troop],
      phase = Phase.PlacingTroops,
      currentPlayer = "" // or some default UserId
    )
    
    val troops = initB(clients)
    var nstate = state
    troops.map { (coord, troop) =>
        println(s"Placing troop ${troop.name} for player ${troop.owner} at ${coord}")
//        val tmpState = nstate.copy(selectedTroop = Some(troop))
//        nstate = GameLogic.handleClick(tmpState, coord)
        nstate = GameLogic.placeTroop(nstate, coord, troop)
    }
    nstate

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
    import Action.*
    println("transition:" + event)
//        val tmpState = nstate.copy(selectedTroop = Some(troop))
//        nstate = GameLogic.handleClick(tmpState, coord)

//    handleClick(state, coord: Coord): State
    Seq(Pause(0))

    // end transition

    // start project
  override def project(state: State)(userId: UserId): View =
    println("project")
    println(ViewLogic.stateToView(state, userId))
    View(ViewLogic.stateToView(state, userId))
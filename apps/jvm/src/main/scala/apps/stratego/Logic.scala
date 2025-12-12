package apps.ul2025app100

import cs214.webapp.UserId
import cs214.webapp.*
import cs214.webapp.server.StateMachine

import scala.util.Try

/** 
  * Server-side logic entry point for the Stratego app.
  *
  * - Holds metadata (appInfo)
  * - Controls initialization (auto-placing pieces)
  * - Validates and applies events in transition
  * - Projects internal State to per-user View
  */
class Logic extends StateMachine[Event, State, View]:

  /** Information shown by the launcher / framework for this app. */
  val appInfo: AppInfo = AppInfo(
    id = "ul2025app100",
    name = "Stratego",
    description = "A simple Stratego-inspired game.",
    year = 2025
  )

  /** Wire used both on server and client to encode Events / Views. */
  override val wire = Wire

  // ---------------------------------------------------------------------------
  // Initial state / setup
  // ---------------------------------------------------------------------------

  /** 
    * Deterministic initial placement:
    *   - player 1: bottom two rows (rows 9 and 8)
    *   - player 2: top two rows   (rows 0 and 1)
    *
    * This keeps the game nicely symmetric without asking the players
    * to do a manual placement phase.
    */
  private def initialPlacement(players: Vector[UserId]): Map[Coord, Troop] =
    val allTroops = GameLogic.initTroops(players(0), players(1))

    // 20 coordinates for each side, laid out row-major
    val p1Coords: Vector[Coord] =
      Vector(9, 8).flatMap(row => (0 until GameLogic.boardSize).map(col => Coord(row, col)))
    val p2Coords: Vector[Coord] =
      Vector(0, 1).flatMap(row => (0 until GameLogic.boardSize).map(col => Coord(row, col)))

    val p1Map = p1Coords.zip(allTroops.take(20)).toMap
    val p2Map = p2Coords.zip(allTroops.drop(20)).toMap

    p1Map ++ p2Map

  /** 
    * Initialize a new game.
    *
    * Expects exactly 2 clients and:
    *   - creates an empty board,
    *   - auto-places all troops for both players,
    *   - starts directly in the Attacking phase.
    */
  override def init(clients: Seq[UserId]): State =
    val players = clients.toVector
    require(players.size == 2, s"Stratego expects exactly 2 players, got ${players.size}")

    val allTroops = GameLogic.initTroops(players(0), players(1)) //All troops
    val p1Troops = util.Random.shuffle(allTroops.take(20)) // player1 shuffled 
    val p2Troops = util.Random.shuffle(allTroops.drop(20)) // player2 shuffled 

    // Base empty state
    val baseState = State(
      board          = GameLogic.emptyBoard(),
      selectedSquare = None,
      selectedTroop  = None,
      dead           = Map.empty[UserId, Set[Troop]],
      players        = players,
      inCombat       = Map.empty[UserId, Troop],
      leftToPlace = Map(players(0) -> p1Troops.toVector, players(1) -> p2Troops.toVector),// each player has to place all the troops for the game to start
      phase          = Phase.PlacingTroops,        
      currentPlayer  = players.head
    )

    /* Auto-place pieces
    val placements = initialPlacement(players)
    val placedState =
      placements.foldLeft(baseState) { case (st, (coord, troop)) =>
        GameLogic.placeTroop(st, coord, troop)
      }*/

    baseState

  // ---------------------------------------------------------------------------
  // Transition
  // ---------------------------------------------------------------------------

  /** 
    * Apply a user event to the current state.
    *
    * Here we:
    *   - block events if the game is over,
    *   - enforce turn-taking,
    *   - delegate most rules to GameLogic.handleClick.
    */
  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try:
    import Action.*

    println(s"transition from $userId: $event")

    // Reject moves if game over
    if state.phase == Phase.Done then
      throw IllegalMoveException("The game is already over!")

    // Only current player may act if we are attacking otherwise both 
    val effectiveState =
      state.phase match
        case Phase.PlacingTroops =>
          // tell GameLogic who is placing right now both players can place at any time 
          state.copy(currentPlayer = userId)
        case _ =>
          if userId != state.currentPlayer then
            throw NotYourTurnException()
          else
            state // attacking 

    event match
      case Event.SquareClicked(coord) =>
        val nextState = GameLogic.handleClick(effectiveState, coord)
        Seq(Render(nextState))

      case Event.KeyPressed(_) =>
        // No keyboard actions yet: just re-render current state
        Seq(Render(state))

  // ---------------------------------------------------------------------------
  // Projection
  // ---------------------------------------------------------------------------

  /** 
    * Project the internal State to a per-user View.
    * Hides opponents' unrevealed pieces and provides highlighting info.
    */
  override def project(state: State)(userId: UserId): View =
    View(ViewLogic.stateToView(state, userId))

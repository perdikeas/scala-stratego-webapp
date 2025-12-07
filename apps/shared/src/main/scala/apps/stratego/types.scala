package apps.stratego

import cs214.webapp.UserId

/** Board coordinates, 0-based indices. */
case class Coord(row: Int, col: Int)

enum Event:
  /** User clicked on a square. */
  case SquareClicked(squareId: Coord)
  /** Optional keyboard events (not used for now). */
  case KeyPressed(key: String)

/** A playing piece on the board. */
case class Troop(
    name: String,
    rank: Int,                 // used for combat resolution
    owner: UserId,             // owning player id
    revealed: Boolean = false  // becomes true after combat
)

/** A single square on the board. */
case class Square(
    coord: Coord,
    troop: Option[Troop]       // None = empty square
)

/** High-level game phase. */
enum Phase:
  case PlacingTroops
  case Attacking
  case ViewingBoard
  case Done

/** 
  * Full internal state of the game.
  *
  * This is never directly sent to clients; instead we project it to
  * a StateView per player.
  */
case class State(
    board: Vector[Square],
    selectedSquare: Option[Coord],        // during attack: which troop we selected
    selectedTroop: Option[Troop],         // during placing (unused for auto-setup)
    dead: Map[UserId, Set[Troop]],        // captured / dead troops (stored as revealed)
    players: Vector[UserId],              // we expect 2 players
    inCombat: Map[UserId, Troop],         // reserved for future extensions
    leftToPlace: Map[UserId, Set[Troop]], // reserved for manual placement mode
    phase: Phase,
    currentPlayer: UserId                 // whose turn it is
):
  /** Returns whether the game is finished or not. */
  def isFinished: Boolean =
    dead.values.flatten.exists(_.name == "Flag")

  /** Returns the winner(s) if the game is finished. */
  def winners: Set[UserId] =
    if !isFinished then Set.empty
    else
      players.filter(p => !dead.getOrElse(p, Set()).exists(_.name == "Flag")).toSet

  /** Switch to the other player. */
  def switchPlayer: State =
    val nextPlayer = players.find(_ != currentPlayer).getOrElse(currentPlayer)
    this.copy(currentPlayer = nextPlayer)

/** Top-level view wrapper sent to the client. */
case class View(
    state: StateView
)

/** Public state projection sent to clients. */
enum StateView:

  /** Placement phase view (board + placement phase info). */
  case Placing(phase: PhaseView, board: Vector[SquareView])

  /** Ongoing game during attack phase. */
  case Playing(
      phase: PhaseView,
      currentPlayer: UserId,
      board: Vector[SquareView],
      selected: Option[Coord],   // which square (if any) is selected
      highlights: Set[Coord]     // legal destinations from selection
  )

  /** The game is over (there may be multiple winners with the same score). */
  case Finished(winnerIds: Set[UserId])

/** Client-side phase detail for the current player. */
enum PhaseView:
  case Selecting           // selecting a piece
  case Attacking           // choosing a destination / target
  case WaitingPlacing      // waiting for other player to place
  case WaitingAttacking    // waiting for other player's move
  case ProperPlacement     // our turn to place

/** How a troop appears from the client's perspective. */
enum TroopView:
  case Covered
  case Uncovered(troop: Troop)
  case DeadView(troop: Troop)

/** What we render at each coordinate. */
enum SquareView:
  case HasTroop(coord: Coord, troop: TroopView)
  case Empty(coord: Coord)

/** Board-level constants shared between logic and UI (e.g., lakes). */
object BoardConstants:
  /** Standard Stratego lakes, using 0-based coordinates. */
  val lakes: Set[Coord] =
    Set(
      Coord(4, 2), Coord(4, 3),
      Coord(5, 2), Coord(5, 3),
      Coord(4, 6), Coord(4, 7),
      Coord(5, 6), Coord(5, 7)
    )

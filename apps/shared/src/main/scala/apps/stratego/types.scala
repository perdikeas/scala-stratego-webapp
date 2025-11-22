package apps.stratego

import cs214.webapp.UserId

case class Coord(row: Int, col: Int) 

enum Event :
  /** User clicked on a square. */
  case SquareClicked(squareId: Coord) 
  //A to alternate between troops while placing or some other 
  //functionality we might add later 
  case KeyPressed(key: String) 

case class Troop (
    name: String,         
    rank: Int,             // used for combat resolution
    owner: UserId,         // who owns it
    revealed: Boolean = false  // becomes true when engaged in combat
)

case class Square(
    coord: Coord,
    troop: Option[Troop]   // None = empty square
)

enum Phase:
  case PlacingTroops
  case Attacking 
  case ViewingBoard
  //maybe we need two Viewing phases one for placing one for attacking
  case Done // Done

case class State(
    board: Vector[Square],
    selectedSquare: Option[Coord], //during attack 
    selectedTroop: Option[Troop], //during placing the selected one 
    dead: Map[UserId,Set[Troop]],
    players: Vector[UserId],//Only 2,
    inCombat: Map[UserId, Troop],
    phase: Phase
);
  /*
  lazy val removed = matched.values.flatten.toSet
  lazy val isFinished = matched.values.map(_.length).sum == cards.length
  lazy val hasCorrectSelection = flipped.size > 1 && flipped.map(cards.apply).size == 1
  */
  
  //lazy val isFinished = dead.values.exist(x => x.name== "flag")
  //lazy val removed = hits

/** A view of the game's state.
  *
  * @param stateView
  *   A projection of the current phase of the game.
  * @param alreadyMatched
  *   The cards that each player has already successfully matched.
  */
case class View(
    state: StateView
)

enum StateView:

  case Placing(phase:PhaseView/*,board:Vector*/)
  /** The game is ongoing. */
  case Playing(phase: PhaseView, currentPlayer: UserId, board: Vector[CardView])

  /** The game is over (there may be multiple winners with the same score). */
  case Finished(winnerIds: UserId)

enum PhaseView:

  case Selecting
  /** It's our turn to pick two cards. */
  case Attacking//case FlippingCards

  /** It's another player's turn: we're waiting for them to flip cards. */
  case WaitingPlacing

  case WaitingAttacking

  case ProperPlacement



enum TroopView:
  case Covered
  case Uncovered(troop: Troop) //selectingPhase
  case DeadView(troop:Troop)

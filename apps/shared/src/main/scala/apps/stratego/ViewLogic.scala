package apps.stratego

import cs214.webapp.UserId

object ViewLogic:

  /** Convert a Troop in the state to a TroopView for rendering. */
  def troopToView(troop: Troop, currentPlayer: UserId, dead: Set[Troop]): TroopView =
    if dead.contains(troop) then TroopView.DeadView(troop)
    else if troop.owner == currentPlayer || troop.revealed then TroopView.Uncovered(troop)
    else TroopView.Covered

  /** Convert the board from State to a board of TroopViews for the current player. */
  def boardToView(board: Vector[Square], currentPlayer: UserId, dead: Map[UserId, Set[Troop]]): Vector[TroopView] =
    board.map { sq =>
      sq.troop match
        case Some(t) => troopToView(t, currentPlayer, dead.getOrElse(t.owner, Set()))
        case None => TroopView.Covered // empty square treated as covered for simplicity
    }

  /** Convert the game State to a StateView for the current player. */
  def stateToView(state: State, currentPlayer: UserId): StateView =
    state.phase match
      case Phase.PlacingTroops =>
        StateView.Placing(
          phase = PhaseView.ProperPlacement,
          board = boardToView(state.board, currentPlayer, state.dead)
        )

      case Phase.Attacking =>
        StateView.Playing(
          phase = if state.selectedSquare.isEmpty then PhaseView.Selecting else PhaseView.Attacking,
          currentPlayer = currentPlayer,
          board = boardToView(state.board, currentPlayer, state.dead)
        )

      case Phase.ViewingBoard =>
        StateView.Playing(
          phase = PhaseView.WaitingAttacking,
          currentPlayer = currentPlayer,
          board = boardToView(state.board, currentPlayer, state.dead)
        )

      case Phase.Done =>
        StateView.Finished(
          winnerIds = state.winners
        )

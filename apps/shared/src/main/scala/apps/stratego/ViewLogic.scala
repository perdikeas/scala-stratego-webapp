package apps.ul2025app100

import cs214.webapp.UserId

/**
  * Converts the internal State (full information) into a StateView
  * that can safely be sent to a given player:
  *
  *  - Hides opponents' unrevealed pieces (shows them as Covered)
  *  - Marks dead pieces as DeadView
  *  - Adds selected square + legal destination highlights
  *  - Encodes current phase from the client's perspective
  */
object ViewLogic:

  /** Convert a Troop in the state to a TroopView for rendering. */
  def troopToView(troop: Troop, currentPlayer: UserId, dead: Set[Troop]): TroopView =
    if dead.contains(troop) then TroopView.DeadView(troop)
    else if troop.owner == currentPlayer || troop.revealed then TroopView.Uncovered(troop)
    else TroopView.Covered

  /** Convert the full board to SquareViews for the current player. */
  def boardToView(state: State, currentPlayer: UserId): Vector[SquareView] =
    state.board.map { sq =>
      sq.troop match
        case Some(t) =>
          SquareView.HasTroop(
            sq.coord,
            troopToView(t, currentPlayer, state.dead.getOrElse(t.owner, Set()))
          )
        case None =>
          SquareView.Empty(sq.coord)
    }

  /** 
    * Convert a full State to a StateView as seen by a specific player.
    * 
    * This is called on every Render(action) by the framework.
    */
  def stateToView(state: State, currentPlayer: UserId): StateView =
    val boardView = boardToView(state, currentPlayer)

    state.phase match
      case Phase.PlacingTroops =>
        val remaining: Vector[Troop] =
          state.leftToPlace.getOrElse(currentPlayer, Vector.empty)

        val phaseView =
          if remaining.nonEmpty then PhaseView.ProperPlacement
          else PhaseView.WaitingPlacing

        val nextTroopOpt = remaining.headOption

        StateView.Placing(
          phase = phaseView,
          board = boardView,
          nextTroop = nextTroopOpt
        )


      // Attacking phase: highlight selections and legal moves
      case Phase.Attacking =>
        val isMyTurn = currentPlayer == state.currentPlayer
        val selectedForUser =
          if isMyTurn then state.selectedSquare else None

        // Highlight legal destinations for selected troop if it's our turn
        val highlightCoords: Set[Coord] =
          selectedForUser match
            case Some(c) if isMyTurn => GameLogic.legalDestinations(state, c)
            case _                   => Set.empty

        val phaseView =
          if !isMyTurn then PhaseView.WaitingAttacking
          else if selectedForUser.isEmpty then PhaseView.Selecting
          else PhaseView.Attacking

        StateView.Playing(
          phase      = phaseView,
          currentPlayer = state.currentPlayer,
          board      = boardView,
          selected   = selectedForUser,
          highlights = highlightCoords
        )

      // ViewingBoard could be used for "replays" or extra board inspection
      case Phase.ViewingBoard =>
        StateView.Playing(
          phase         = PhaseView.WaitingAttacking,
          currentPlayer = state.currentPlayer,
          board         = boardView,
          selected      = None,
          highlights    = Set.empty
        )

      // Game is finished: send winners
      case Phase.Done =>
        StateView.Finished(
          winnerIds = state.winners
        )

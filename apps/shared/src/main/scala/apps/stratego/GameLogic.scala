package apps.ul2025app100

import cs214.webapp.UserId

/**
  * Pure game mechanics for Stratego:
  *   - board creation and troop initialization
  *   - movement rules (including lakes and scouts)
  *   - combat resolution and fatalities
  *   - click handling for the Attacking phase
  *
  * This file is entirely independent of networking / rendering.
  */
object GameLogic:

  /** Side length of the (square) board. */
  val boardSize = 10

  /** Creates an empty 10x10 board of Square(row, col, None). */
  def emptyBoard(): Vector[Square] =
    (for
      row <- 0 until boardSize
      col <- 0 until boardSize
    yield Square(Coord(row, col), None)).toVector

  /** 
    * Initialize the 20 troops for each player.
    * 
    * We return a flat vector: first 20 belong to player1, the remaining
    * 20 are copies assigned to player2.
    */
  def initTroops(player1: UserId, player2: UserId): Vector[Troop] =
    val baseTroops = Vector(
      Troop("Flag", 0, player1),
      Troop("Spy", 1, player1),
      Troop("Scout", 2, player1),
      Troop("Scout", 2, player1),
      Troop("Miner", 3, player1),
      Troop("Miner", 3, player1),
      Troop("Sergeant", 4, player1),
      Troop("Sergeant", 4, player1),
      Troop("Lieutenant", 5, player1),
      Troop("Lieutenant", 5, player1),
      Troop("Captain", 6, player1),
      Troop("Captain", 6, player1),
      Troop("Major", 7, player1),
      Troop("Major", 7, player1),
      Troop("Colonel", 8, player1),
      Troop("Colonel", 8, player1),
      Troop("General", 9, player1),
      Troop("Marshal", 10, player1),
      Troop("Bomb", 11, player1),
      Troop("Bomb", 11, player1)
    )

    val p1Troops = baseTroops
    val p2Troops = baseTroops.map(t => t.copy(owner = player2))
    p1Troops ++ p2Troops

  /** 
    * Place a troop on the board at a given coordinate.
    * 
    * Used both by initialization and (optionally) by a manual placement phase.
    */
  def placeTroop(state: State, coord: Coord, troop: Troop): State =
    val updatedBoard = state.board.map { sq =>
      if sq.coord == coord then sq.copy(troop = Some(troop))
      else sq
    }
    val remainingTroop =
      if state.selectedTroop.contains(troop) then None else state.selectedTroop
    state.copy(board = updatedBoard, selectedTroop = remainingTroop)

  /** 
    * Resolve a combat between attacker and defender according to Stratego rules.
    *
    * Returns a pair (attackerSurvives, defenderSurvives) where each element
    * is Some(troop) if that troop survived, or None if it died.
    */
  def resolveCombat(attacker: Troop, defender: Troop): (Option[Troop], Option[Troop]) =
    (attacker.name, defender.name, attacker.rank, defender.rank) match
      // Spy special case
      case ("Spy", "Marshal", 1, 10) => (Some(attacker), None) // Spy attacks Marshal → Spy wins
      case ("Marshal", "Spy", 10, 1) => (None, Some(defender)) // Marshal attacks Spy → Marshal wins

      // Bomb rules
      case (_, "Bomb", _, _) if attacker.name == "Miner" =>
        (Some(attacker), None)                                // Miner defuses Bomb
      case (_, "Bomb", _, _) =>
        (None, Some(defender))                                // Any other attacker dies
      case ("Bomb", _, _, _) =>
        (None, Some(defender))                                // Bomb cannot move, but if attacked, attacker dies

      // Normal rank comparison
      case (_, _, a, d) if a > d  => (Some(attacker), None)
      case (_, _, a, d) if a < d  => (None, Some(defender))
      case (_, _, a, d) if a == d => (None, None)             // tie, both die

      case _ => (Some(attacker), Some(defender))

  /** Checks if the game is finished and updates the phase accordingly. */
  def checkGameOver(state: State): State =
    if state.isFinished then state.copy(phase = Phase.Done)
    else state

  // ---------------------------------------------------------------------------
  // Movement helpers (with lakes and scouts)
  // ---------------------------------------------------------------------------

  /** True if the coordinate is within board bounds. */
  def isInsideBoard(c: Coord): Boolean =
    c.row >= 0 && c.row < boardSize && c.col >= 0 && c.col < boardSize

  /** True if the coordinate is a lake square. */
  def isLake(c: Coord): Boolean =
    BoardConstants.lakes.contains(c)

  /**
    * True if the troop on `from` can reach `to` following Stratego movement rules,
    * ignoring whether `to` is occupied or not.
    */
  def canReach(from: Coord, to: Coord, state: State): Boolean =
    if from == to then return false
    if !isInsideBoard(from) || !isInsideBoard(to) then return false
    if isLake(from) || isLake(to) then return false

    val maybeTroop = state.board.find(_.coord == from).flatMap(_.troop)
    if maybeTroop.isEmpty then return false
    val troop = maybeTroop.get

    // Bombs and Flags cannot move
    if troop.name == "Bomb" || troop.name == "Flag" then return false

    val dx = math.abs(from.col - to.col)
    val dy = math.abs(from.row - to.row)

    if troop.name == "Scout" then
      // Scouts move any distance in straight line, no pieces or lakes in the way
      if dx != 0 && dy != 0 then return false    // must be straight line

      val pathCoords =
        if dx != 0 then
          val step = if to.col > from.col then 1 else -1
          (from.col + step until to.col by step).map(col => Coord(from.row, col))
        else
          val step = if to.row > from.row then 1 else -1
          (from.row + step until to.row by step).map(row => Coord(row, from.col))

      pathCoords.forall { c =>
        !isLake(c) &&
        state.board.find(_.coord == c).flatMap(_.troop).isEmpty
      }
    else
      // Other movable pieces: exactly one step orthogonally, not into a lake
      ((dx == 1 && dy == 0) || (dx == 0 && dy == 1)) && !isLake(to)

  /** True if moving from -> to is a legal non-attacking move (destination empty). */
  def isLegalMove(from: Coord, to: Coord, state: State): Boolean =
    if !canReach(from, to, state) then false
    else
      val dest = state.board.find(_.coord == to).get
      dest.troop.isEmpty

  /** True if from can legally attack to (destination must contain an enemy). */
  def isLegalAttack(from: Coord, to: Coord, state: State, currentPlayer: UserId): Boolean =
    if !canReach(from, to, state) then false
    else
      val dest = state.board.find(_.coord == to).get
      dest.troop.exists(_.owner != currentPlayer)

  /** All legal destinations (moves or attacks) for the troop on `from`. */
  def legalDestinations(state: State, from: Coord): Set[Coord] =
    state.board.iterator
      .map(_.coord)
      .filter { to =>
        isLegalMove(from, to, state) ||
        isLegalAttack(from, to, state, state.currentPlayer)
      }
      .toSet

  // ---------------------------------------------------------------------------
  // Handle clicks (core game loop for the Attacking phase)
  // ---------------------------------------------------------------------------

  /**
    * Handle a click on the board depending on the current phase.
    *
    * We only support the Attacking phase interactively in this version:
    * pieces are auto-placed, so PlacingTroops is a no-op.
    */
  def handleClick(state: State, coord: Coord): State =
    val newState =
      state.phase match
        // We skip interactive placement in this version: troops are auto-placed
        case Phase.PlacingTroops =>
          val myTroops :  Vector[Troop] = state.leftToPlace.getOrElse(state.currentPlayer,Vector.empty)
          if myTroops.isEmpty then state
          else
          /// Clicked coordinate validity check
            val maybeSquare = state.board.find(_.coord == coord)
            if maybeSquare.isEmpty then state //if we clicked something else or nothing 
            else
              val square = maybeSquare.get 
              // its an option so now we get the clicked square and can check if it has a troop inside 

              // don't allow placing on occupied or lake squares
              val occupied = square.troop.nonEmpty
              val invalidTerrain = isLake(coord)

              // only allow in your half of the board and the first two rows 
              val validZone =
                if state.currentPlayer == state.players(0) then coord.row >= 8 // bottom 2 rows
                else coord.row <= 1                                            // top 2 rows

              if occupied || invalidTerrain || !validZone then
                state // we ignore invalid clicks
              else
                //get the troop to place
                val troop = myTroops.head
                val newBoard = state.board.map { sq =>
                  if sq.coord == coord then sq.copy(troop = Some(troop))
                  else sq
                }
                val newLeft = myTroops.tail
                val updatedLeft = state.leftToPlace.updated(state.currentPlayer, newLeft)

                      // Check if both players finished placing
                val bothDone =
                  updatedLeft.values.forall(_.isEmpty)
                if bothDone then
                  state.copy(
                    board = newBoard,
                    leftToPlace = updatedLeft,
                    phase = Phase.Attacking,
                    currentPlayer = state.players.head
                  )
                else
                  // stay in placing phase, no switch
                  state.copy(board = newBoard, leftToPlace = updatedLeft)

        // ATTACK PHASE: selection, movement, combat
        case Phase.Attacking =>
          val maybeSquare  = state.board.find(_.coord == coord)
          val maybeTarget  = maybeSquare.flatMap(_.troop)
          val selectedOpt  = state.selectedSquare

          (maybeTarget, selectedOpt) match

            // 0. Clicking again on the selected square cancels selection
            case (_, Some(sel)) if coord == sel =>
              state.copy(selectedSquare = None)

            // 1. MOVE selected troop to an EMPTY square
            case (None, Some(attackerCoord)) =>
              if isLake(coord) then state
              else
                val maybeAttacker = state.board.find(_.coord == attackerCoord).flatMap(_.troop)
                maybeAttacker match
                  case Some(attacker) =>
                    if !isLegalMove(attackerCoord, coord, state) then state
                    else
                      val updatedBoard =
                        state.board.map { sq =>
                          if sq.coord == attackerCoord then sq.copy(troop = None)
                          else if sq.coord == coord then sq.copy(troop = Some(attacker))
                          else sq
                        }

                      state.copy(
                        board = updatedBoard,
                        selectedSquare = None
                      ).switchPlayer
                  case None =>
                    state

            // 2. ATTACK an enemy with selected attacker
            case (Some(target), Some(attackerCoord)) =>
              val maybeAttacker = state.board.find(_.coord == attackerCoord).flatMap(_.troop)

              maybeAttacker match
                case Some(attacker) =>
                  // Can't move Bombs or Flags
                  if attacker.name == "Bomb" || attacker.name == "Flag" then
                    state
                  else if !isLegalAttack(attackerCoord, coord, state, state.currentPlayer) then
                    state
                  else
                    // Reveal both troops on combat
                    val attackerRevealed = attacker.copy(revealed = true)
                    val targetRevealed   = target.copy(revealed = true)

                    val (attSurvOpt, defSurvOpt) =
                      resolveCombat(attackerRevealed, targetRevealed)

                    val updatedBoard =
                      state.board.map { sq =>
                        if sq.coord == attackerCoord then sq.copy(troop = attSurvOpt)
                        else if sq.coord == coord then sq.copy(troop = defSurvOpt)
                        else sq
                      }

                    // Update dead map for any troops that died (store as revealed)
                    val updatedDead =
                      Seq(attSurvOpt -> attackerRevealed, defSurvOpt -> targetRevealed)
                        .foldLeft(state.dead) { case (deadMap, (survOpt, orig)) =>
                          survOpt match
                            case Some(_) => deadMap
                            case None =>
                              val set = deadMap.getOrElse(orig.owner, Set())
                              deadMap.updated(orig.owner, set + orig)
                        }

                    state.copy(
                      board = updatedBoard,
                      dead = updatedDead,
                      selectedSquare = None
                    ).switchPlayer

                case None =>
                  state

            // 3. SELECT your troop to prepare attack/move
            case (Some(target), None)
                if target.owner == state.currentPlayer &&
                   target.name != "Bomb" &&
                   target.name != "Flag" &&
                   !isLake(coord) =>
              state.copy(selectedSquare = Some(coord))

            // 4. Clicking enemy troop without attacker → ignore
            case (Some(_), None) =>
              state

            // 5. Clicking empty with no selection → ignore
            case _ =>
              state

        case Phase.ViewingBoard =>
          state

        case Phase.Done =>
          state

    checkGameOver(newState)

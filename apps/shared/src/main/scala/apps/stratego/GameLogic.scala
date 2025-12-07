package apps.stratego
import cs214.webapp.UserId

object GameLogic:

    val boardSize = 10

    /** Creates an empty 10x10 board. */
    def emptyBoard(): Vector[Square] =
        (for {
        row <- 0 until boardSize
        col <- 0 until boardSize
        } yield Square(Coord(row, col), None)).toVector

    /** Initialize troops for both players. */
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

    /** Place a troop on the board during setup phase. */
    def placeTroop(state: State, coord: Coord, troop: Troop): State =
        val updatedBoard = state.board.map { sq =>
        if sq.coord == coord then sq.copy(troop = Some(troop))
        else sq
        }
        val remainingTroop = if state.selectedTroop.contains(troop) then None else state.selectedTroop
        state.copy(board = updatedBoard, selectedTroop = remainingTroop)

    /** Resolve a combat between attacker and defender. */
    def resolveCombat(attacker: Troop, defender: Troop): (Option[Troop], Option[Troop]) =
        (attacker.name, defender.name, attacker.rank, defender.rank) match
            // Spy special case
            case ("Spy", "Marshal", 1, 10) => (Some(attacker), None) // Spy attacks Marshal → Spy wins
            case ("Marshal", "Spy", 10, 1) => (None, Some(defender)) // Marshal attacks Spy → Marshal wins

            // Bomb rules
            case (_, "Bomb", _, _) if attacker.name == "Miner" => (Some(attacker), None) // Miner defuses Bomb
            case (_, "Bomb", _, _) => (None, Some(defender)) // Any other attacker dies

            case ("Bomb", _, _, _) => (None, Some(defender)) // Bomb cannot move → dies if attacked? optional

            // Normal rank comparison
            case (_, _, a, d) if a > d => (Some(attacker), None)
            case (_, _, a, d) if a < d => (None, Some(defender))
            case (_, _, a, d) if a == d => (None, None) // tie, both die

            // Default (should not happen)
            case _ => (Some(attacker), Some(defender))

    /** Checks if the game is finished and updates the phase accordingly */
    def checkGameOver(state: State): State =
        if state.isFinished then state.copy(phase = Phase.Done)
        else state

    /** Handle a click event on the board depending on phase. */
    def handleClick(state: State, coord: Coord): State =
        val newState = 
            state.phase match
                //Placement phase
                    case Phase.PlacingTroops => 
                        state.selectedTroop match
                            case Some(t) if t.owner==state.currentPlayer => 
                                //restrict placement to the player's starting rows
                                val rowValid = if state.currentPlayer == state.players.head then coord.row < 4
                                    else coord.row >= 6
                                if !rowValid then state // invalid row → do nothing
                                else 
                                    val tmpState = placeTroop(state,coord,t)
                                    //check if all troops for this player have been placed
                                    val allPlaced = tmpState.board.count(_.troop.exists(_.owner == state.currentPlayer)) >= 20
                                    if allPlaced then 
                                        // If all troops placed, either move to next phase or continue
                                        val nextPhase = 
                                            if state.currentPlayer == state.players.last then 
                                                Phase.Attacking else Phase.PlacingTroops
                                        tmpState.copy(phase = nextPhase).switchPlayer
                                    else 
                                        tmpState.switchPlayer
                            case _ =>
                                state //no troop selected -> do nothing
                    
                //Attack phase
                    case Phase.Attacking => 
                        val maybeTarget = state.board.find(_.coord == coord).flatMap(_.troop)
                        (maybeTarget,state.selectedSquare) match 
                            //attacking a selected target
                            case (Some(target), Some(attackerCoord)) => 
                                val maybeAttacker = state.board.find(_.coord == attackerCoord).flatMap(_.troop)
                                maybeAttacker match 
                                    case Some(attacker) => 
                                        // Bombs and Flags cannot attack
                                        if attacker.name == "Bomb" || attacker.name == "Flag" then
                                            state
                                        else
                                            val (attackerSurvives, defenderSurvives) = resolveCombat(attacker, target)
                                            // Update the board
                                            val updatedBoard = state.board.map{
                                                sq => if sq.coord==attackerCoord then sq.copy(troop = attackerSurvives)
                                                else if sq.coord == coord then sq.copy(troop = defenderSurvives)
                                                else sq
                                            }
                                            //Update dead map for any troops that died
                                            val updatedDead = Seq(attackerSurvives, defenderSurvives)
                                                .zip(Seq(attacker, target))
                                                .foldLeft(state.dead) {
                                                case (deadMap, (survivorOpt, original)) =>
                                                survivorOpt match
                                                case Some(_) => deadMap // survived → not dead
                                                case None => deadMap.updated(original.owner, deadMap.getOrElse(original.owner, Set()) + original)
                                                }
                                            // Reset selection and switch turn
                                            state.copy(board = updatedBoard, dead = updatedDead, selectedSquare = None).switchPlayer
                                    case None =>
                                        state // attacker not found → do nothing 

                            // Select an attacker (only your troops, not Bombs or Flags)
                            case (Some(target), None) if target.owner == state.currentPlayer &&
                            target.name != "Bomb" && target.name != "Flag" =>
                                        state.copy(selectedSquare = Some(coord))     

                            // Clicked opponent troop without selecting attacker → ignore
                            case (Some(_), None) =>
                                state   

                            //Clicked empty square -> ignore
                            case _ => 
                                state     
                    //other phases: ViewingBoard,Done, etc
                    case _ => state 

        //After handling the click, check if the game is finished
        checkGameOver(newState)
    
/*????????????This exists twice also in View Logic I am commenting 
this for now  possible removal???????????????????????????????????????????????*/
    /**Convert the internal game State into a View-friendly StateView 
    def stateToView(state: State, currentPlayer: UserId): StateView =
        // Helper to convert a single troop into TroopView
        def troopToView(t:Troop):TroopView = 
            if state.dead.getOrElse(t.owner, Set()).contains(t) then TroopView.DeadView(t)
            else if t.owner == currentPlayer || t.revealed then TroopView.Uncovered(t)
            else TroopView.Covered
        
        // Convert the board squares into TroopViews (preserve order)
        val boardView: Vector[TroopView] = state.board.map { sq =>
            sq.troop match
                case Some(t) => troopToView(t)
                case None => TroopView.Covered // empty square treated as covered
        }
        
        // Determine which StateView to return based on game phase
        state.phase match
            case Phase.PlacingTroops =>
                StateView.Placing(PhaseView.Selecting, boardView)
        
            case Phase.Attacking =>
                StateView.Playing(
                    phase = PhaseView.Attacking,
                    currentPlayer = currentPlayer,
                    board = boardView
                )

            case Phase.ViewingBoard =>
                StateView.Playing(
                    phase = PhaseView.WaitingAttacking,
                    currentPlayer = currentPlayer,
                    board = boardView
                )

            case Phase.Done =>
                StateView.Finished(winnerIds = state.winners)*/

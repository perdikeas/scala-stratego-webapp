package apps.ul2025app100

import cs214.webapp.*
import cs214.webapp.Action
import cs214.webapp.utils.WebappSuite

class Tests extends WebappSuite[Event, State, View]:

  val sm = Logic()

  // ============================================================
  // IMPORTANT: Logic.init REQUIRES EXACTLY 2 PLAYERS
  // ============================================================
  private val PLAYERS = USER_IDS.take(2)
  private val p0 = PLAYERS(0)
  private val p1 = PLAYERS(1)

  // ============================================================
  // Helpers
  // ============================================================

  private def step(state: State, user: UserId, coord: Coord): State =
    val actions = sm.transition(state)(user, Event.SquareClicked(coord)).get
    actions.collectFirst { case Action.Render(st) => st }
      .getOrElse(fail(s"Expected Render action, got $actions"))

  private def boardTroopCount(state: State): Int =
    state.board.count(_.troop.nonEmpty)

  private def troopAt(state: State, c: Coord): Option[Troop] =
    state.board.find(_.coord == c).flatMap(_.troop)

  private def p0PlacementCoords: Vector[Coord] =
    (for row <- Vector(9, 8); col <- 0 until GameLogic.boardSize yield Coord(row, col)).toVector

  private def p1PlacementCoords: Vector[Coord] =
    (for row <- Vector(0, 1); col <- 0 until GameLogic.boardSize yield Coord(row, col)).toVector

  private def mkAttackingState(): State =
    State(
      board          = GameLogic.emptyBoard(),
      selectedSquare = None,
      selectedTroop  = None,
      dead           = Map.empty,
      players        = Vector(p0, p1),
      inCombat       = Map.empty,
      leftToPlace    = Map.empty,
      phase          = Phase.Attacking,
      currentPlayer  = p0
    )

  private def withTroop(st: State, c: Coord, t: Troop): State =
    GameLogic.placeTroop(st, c, t)

  // ============================================================
  // INIT / PLACEMENT (FIXED TESTS)
  // ============================================================

  test("init: starts in PlacingTroops with an empty board"):
    val initState = sm.init(PLAYERS)
    assertEquals(initState.phase, Phase.PlacingTroops)
    assertEquals(boardTroopCount(initState), 0)
    assertEquals(initState.board.size, GameLogic.boardSize * GameLogic.boardSize)

  test("init: leftToPlace contains 20 troops per player with correct ownership"):
    val initState = sm.init(PLAYERS)

    val p0Left = initState.leftToPlace.getOrElse(p0, Vector.empty)
    val p1Left = initState.leftToPlace.getOrElse(p1, Vector.empty)

    assertEquals(p0Left.size, 20)
    assertEquals(p1Left.size, 20)

    assert(p0Left.forall(_.owner == p0))
    assert(p1Left.forall(_.owner == p1))

    // Total troops invariant
    val allTroops = p0Left ++ p1Left
    assertEquals(allTroops.size, 40)

    // Rank multiset invariant (independent of order & ownership)
    val expectedRanks =
      GameLogic.initTroops(p0, p1).map(_.rank).sorted
    val actualRanks =
      allTroops.map(_.rank).sorted

    assertEquals(actualRanks, expectedRanks)

  test("placing: clicking outside your placement zone does nothing"):
    var st = sm.init(PLAYERS)

    val beforeBoard = boardTroopCount(st)
    val beforeLeft  = st.leftToPlace(p0).size

    st = step(st, p0, Coord(0, 0)) // invalid for p0

    assertEquals(boardTroopCount(st), beforeBoard)
    assertEquals(st.leftToPlace(p0).size, beforeLeft)

  test("placing: clicking same valid square twice only places once"):
    var st = sm.init(PLAYERS)
    val c  = Coord(9, 0)

    val beforeLeft = st.leftToPlace(p0).size
    st = step(st, p0, c)

    assertEquals(boardTroopCount(st), 1)
    assertEquals(st.leftToPlace(p0).size, beforeLeft - 1)
    assert(troopAt(st, c).nonEmpty)

    val afterLeft = st.leftToPlace(p0).size
    st = step(st, p0, c)

    assertEquals(boardTroopCount(st), 1)
    assertEquals(st.leftToPlace(p0).size, afterLeft)

  // ============================================================
  // ATTACKING PHASE (UNCHANGED – THESE ALREADY WORKED)
  // ============================================================

  test("attacking: selecting and deselecting a movable troop"):
    var st = mkAttackingState()
    val c = Coord(6, 0)

    st = withTroop(st, c, Troop("Sergeant", 4, p0))
    st = step(st, p0, c)
    assertEquals(st.selectedSquare, Some(c))

    st = step(st, p0, c)
    assertEquals(st.selectedSquare, None)

  test("attacking: Bombs and Flags cannot be selected"):
    var st = mkAttackingState()

    st = withTroop(st, Coord(6, 0), Troop("Bomb", 11, p0))
    st = withTroop(st, Coord(6, 1), Troop("Flag", 0, p0))

    st = step(st, p0, Coord(6, 0))
    assertEquals(st.selectedSquare, None)

    st = step(st, p0, Coord(6, 1))
    assertEquals(st.selectedSquare, None)

  test("attacking: normal piece moves one square and switches turn"):
    var st = mkAttackingState()
    val from = Coord(6, 0)
    val to   = Coord(5, 0)

    st = withTroop(st, from, Troop("Sergeant", 4, p0))
    st = step(st, p0, from)
    st = step(st, p0, to)

    assert(troopAt(st, to).exists(_.owner == p0))
    assertEquals(st.currentPlayer, p1)

  test("attacking: scout moves multiple squares"):
    var st = mkAttackingState()
    val from = Coord(6, 0)
    val to   = Coord(6, 5)

    st = withTroop(st, from, Troop("Scout", 2, p0))
    st = step(st, p0, from)
    st = step(st, p0, to)

    assert(troopAt(st, to).exists(_.name == "Scout"))

  // ============================================================
  // COMBAT (PURE LOGIC – UNCHANGED)
  // ============================================================

  test("combat: spy kills marshal when attacking"):
    val (a, d) = GameLogic.resolveCombat(
      Troop("Spy", 1, p0, revealed = true),
      Troop("Marshal", 10, p1, revealed = true)
    )
    assert(a.nonEmpty)
    assert(d.isEmpty)

  test("combat: miner defuses bomb"):
    val (a, d) = GameLogic.resolveCombat(
      Troop("Miner", 3, p0, revealed = true),
      Troop("Bomb", 11, p1, revealed = true)
    )
    assert(a.nonEmpty)
    assert(d.isEmpty)

  test("combat: equal ranks both die"):
    val (a, d) = GameLogic.resolveCombat(
      Troop("Captain", 6, p0, revealed = true),
      Troop("Captain", 6, p1, revealed = true)
    )
    assert(a.isEmpty)
    assert(d.isEmpty)

    //Final tests: turn enforcement and game ending

  test("attacking: wrong player cannot act"):
    var st = mkAttackingState()

    val c = Coord(6, 0)
    st = withTroop(st, c, Troop("Sergeant", 4, p0))

    val res = sm.transition(st)(p1, Event.SquareClicked(c))
    assert(res.isFailure)

  test("game ends when a Flag is captured"):
    val flag = Troop("Flag", 0, p1, revealed = true)

    val st = State(
      board          = GameLogic.emptyBoard(),
      selectedSquare = None,
      selectedTroop  = None,
      dead           = Map(p1 -> Set(flag)),
      players        = Vector(p0, p1),
      inCombat       = Map.empty,
      leftToPlace    = Map.empty,
      phase          = Phase.Attacking,
      currentPlayer  = p0
    )

    val end = GameLogic.checkGameOver(st)

    assertEquals(end.phase, Phase.Done)
    assert(end.winners.contains(p0))
  


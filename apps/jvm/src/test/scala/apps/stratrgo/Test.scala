package apps.stratrgo

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite

class Tests extends WebappSuite[Event, State, View]:
  val sm = Logic()

  /** Projects a given state for each given player and extract the [[state]]
    * field of the result.
    */
  def projectPlayingViews(userIds: Seq[UserId])(state: State) =
    USER_IDS
      .map(sm.project(state))
      .map(_.state.assertInstanceOf[StateView.Playing])

  /** Projects a given state for each given player and extracts the [[matched]]
    * scores table.
    */
  def projectMatched(userIds: Seq[UserId])(state: State) =
    userIds
      .map(sm.project(state))
      .map(_.matched)

  case class FlipResult(
      idx1: Int,
      idx2: Int,
      card1: Card,
      card2: Card,
      nCards: Int,
      firstFlip: State,
      secondFlip: Seq[Action[State]]
  ):
    def isMatch = card1 == card2

    def cards = (card1, card2)

    def stateWithCardsShown =
      assert(secondFlip.nonEmpty)
      secondFlip.head.assertInstanceOf[Action.Render[State]].st

    def stateWithCardsHidden =
      assert(secondFlip.nonEmpty)
      secondFlip.last.assertInstanceOf[Action.Render[State]].st

    def allStates =
      firstFlip +: secondFlip.collect { case Action.Render(st) => st }

  /** Makes a user flip two cards and returns the cards that were flipped and
    * whether they were a match or not.
    */
  def flipTwoCards(state: State, userId: UserId, cards: (Int, Int)): FlipResult =
    val firstFlip = assertSingleRender:
      sm.transition(state)(userId, Event.CardClicked(cards._1))
    val secondFlip = assertSuccess:
      sm.transition(firstFlip)(userId, Event.CardClicked(cards._2))

    assert(secondFlip.nonEmpty)

    val displayedState = secondFlip.head.asInstanceOf[Action.Render[State]].st
    val playingStateView = sm.project(displayedState)(userId)
      .state.assertInstanceOf[StateView.Playing]

    FlipResult(
      idx1 = cards._1,
      idx2 = cards._2,
      card1 = playingStateView.board(cards._1).assertInstanceOf[CardView.FaceUp].card,
      card2 = playingStateView.board(cards._2).assertInstanceOf[CardView.FaceUp].card,
      nCards = playingStateView.board.size,
      firstFlip = firstFlip,
      secondFlip = secondFlip
    )

  def boardSize(state: State): Int =
    sm.project(state)(UID0).state.assertInstanceOf[StateView.Playing].board.size

  /* Cheat by looking at all the cards to facilitate testing. */
  def guessCards(state: State): Seq[Card] =
    val nCards = boardSize(state)
    assert(nCards > 1)
    (0 until nCards).map: idx =>
      flipTwoCards(state, UID0, (idx, (idx + 1) % nCards)).card1

  /* Group indices by card. */
  def findPairs(state: State): Seq[(Int, Int)] =
    val cards = guessCards(state)
    for
      (card, indices) <- (0 until cards.length).groupBy(cards).toSeq
      _ = assert(indices.length % 2 == 0, "There should be an even number of each card.")
      pair <- indices.grouped(2)
    yield (pair(0), pair(1))

/// # Unit tests

/// ## Initial state

  lazy val initState = sm.init(USER_IDS)

  test("Memory: Initial state has all cards face down (2pts)"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for view <- views do
      assertEquals(view.board.size, Logic.CARDS.size * 2)
      for card <- view.board do
        assertEquals(card, CardView.FaceDown)

  test("Memory: Initial state has all players at score 0 (2pts)"):
    val scores = projectMatched(USER_IDS)(initState)

    for matchedCards <- scores do
      assertEquals(matchedCards.keys.toSet, USER_IDS.toSet)
      for userId <- USER_IDS do
        assertEquals(matchedCards(userId).size, 0)

  test("Memory: Initial state has correct initial player right number of cards (1pts)"):
    val views = projectPlayingViews(USER_IDS)(initState)

    for playingView <- views do
      assertEquals(playingView.currentPlayer, UID0)
      // There must be an even number of cards to win, and we need 4 cards to test
      assert(playingView.board.length % 2 == 0 && playingView.board.length >= 4)

/// ## Playing state

  def flipTwoRandomCards(
      matching: Boolean,
      state: State = initState,
      userId: UserId = UID0
  ) =
    val pairs = RNG.shuffle(findPairs(initState))
    assert(pairs.length > 1, "There should be at least two different cards!")
    val (idx0, idx1) =
      if matching then pairs(0)
      else (pairs(0)._1, pairs(1)._1)
    flipTwoCards(state, userId, (idx0, idx1))

  test("Memory: Playing state should let the player flip one card and mark it as flipped (4pts)"):
    val flipped = assertSingleRender:
      sm.transition(initState)(UID0, Event.CardClicked(0))

    for playingView <- projectPlayingViews(USER_IDS)(flipped) do
      playingView.board(0).assertInstanceOf[CardView.FaceUp]

  test("Memory: Nothing should happen when flipping an already flipped card in playing state (2pts)"):
    val flipped = assertSingleRender:
      sm.transition(initState)(UID0, Event.CardClicked(0))
    val flipped2 = assertSingleRender:
      sm.transition(flipped)(UID0, Event.CardClicked(0))
    for playingView <- projectPlayingViews(USER_IDS)(flipped2) do
      playingView.board(0).assertInstanceOf[CardView.FaceUp]

  def testFlipStates(afterFlip: FlipResult) =
    val cards = guessCards(initState)

    // Three actions: render flipped, wait, render hidden
    assertEquals(afterFlip.secondFlip.length, 3)

    // The cards are face up for the two players
    for StateView.Playing(phase, currentPlayer, board) <- projectPlayingViews(USER_IDS)(afterFlip.stateWithCardsShown)
    do
      assertEquals(board.size, cards.length)
      board(afterFlip.idx1).assertInstanceOf[CardView.FaceUp].card
      board(afterFlip.idx2).assertInstanceOf[CardView.FaceUp].card
      for
        idx <- 0 until board.length
        if idx != afterFlip.idx1 && idx != afterFlip.idx2
      do
        board(idx) == CardView.FaceDown

    // Check that we have a proper pause
    val Action.Pause(durationMs) = afterFlip.secondFlip(1).assertInstanceOf[Action.Pause[State]]
    assert(durationMs > 100, "Too fast!")

    // Assert that the two cards are face down at the end
    val lastActionState = afterFlip.stateWithCardsHidden
    for StateView.Playing(phase, currentPlayer, board) <- projectPlayingViews(USER_IDS)(lastActionState) do
      board.forall(_ == CardView.FaceDown)

  test(
    "Memory: Playing state show the two cards when flipped, pause, and hide the two cards (9pts)"
  ):
    val afterFlip = flipTwoRandomCards(matching = false)
    testFlipStates(afterFlip)

  test("Memory: Playing state should update the ScoresView if the cards are a match (2pts)"):
    for (userId, userIdx) <- USER_IDS.zipWithIndex do
      var st = initState
      // Skip others' turns
      for otherId <- USER_IDS.take(userIdx) do
        st = flipTwoRandomCards(matching = false, state = st, userId = otherId).stateWithCardsHidden
      // Find a match when it comes to userId's turn
      var afterFlip = flipTwoRandomCards(matching = true, state = st, userId = userId)
      for tricks <- projectMatched(USER_IDS)(afterFlip.stateWithCardsHidden) do
        assertEquals(
          tricks,
          USER_IDS.map(_ -> Vector()).toMap + (userId -> Vector(afterFlip.card1, afterFlip.card2))
        )

  test("Memory: Playing state should leave scores unchanged if flipped cards don't match (2pts)"):
    var state = initState
    for (userId, pairs) <- USER_IDS.zip(findPairs(initState).sliding(2)) do
      val afterFlip = flipTwoCards(state, userId, (pairs(0)._1, pairs(1)._1))
      state = afterFlip.stateWithCardsHidden
      for tricks <- projectMatched(USER_IDS)(afterFlip.stateWithCardsHidden) do
        assertEquals(tricks, USER_IDS.map(_ -> Vector()).toMap)

  def playEntireGame(initState: State) =
    val pairs = RNG.shuffle(findPairs(initState))

    val firstFlip = flipTwoCards(initState, UID0, pairs.head)

    // Make the first player do all the moves
    pairs.tail.scanLeft(firstFlip) { case (flip, (idx1, idx2)) =>
      flipTwoCards(flip.stateWithCardsHidden, UID0, (idx1, idx2))
    }

  test("Memory: Playing state should first show that cards are matching before declaring a winner (2pts)"):
    val flips = playEntireGame(initState)
    val lastFlip = flips.last

    // Penultimate state should be revealing the cards
    val flipViews = projectPlayingViews(USER_IDS)(lastFlip.stateWithCardsShown)
    for flipView <- flipViews do
      assertEquals(flipView.phase, PhaseView.GoodMatch)

    // Check score
    val expected = USER_IDS.map(_ -> Vector()).toMap + (UID0 -> flips.flatMap(_.cards.toList).sorted)
    for am <- projectMatched(USER_IDS)(lastFlip.stateWithCardsHidden) do
      assertEquals(am.map((uid, cards) => (uid, cards.sorted)), expected)

    // Last state should be showing the winner
    val winViews = USER_IDS.map(sm.project(lastFlip.stateWithCardsHidden))
    for winView <- winViews do
      winView.state.assertInstanceOf[StateView.Finished]

  test("Memory: Playing state should let the next player play when two cards have not been correctly matched (4pts)"):
    val afterFlip = flipTwoRandomCards(matching = false)
    val currentPlayer = projectPlayingViews(USER_IDS)(afterFlip.stateWithCardsHidden)(0).currentPlayer
    assertEquals(currentPlayer, UID1)

  test("Memory: Playing state should let the same player play again when two cards have been correctly matched (4pts)"):
    val afterFlip = flipTwoRandomCards(matching = true)
    val currentPlayer = projectPlayingViews(USER_IDS)(afterFlip.stateWithCardsHidden)(0).currentPlayer
    assertEquals(currentPlayer, UID0)

/// ## Won state

  test("Memory: Won state should prevent any playing interaction by displaying an error (1pts)"):
    val lastState = playEntireGame(initState).last.stateWithCardsHidden

    for userId <- USER_IDS do
      assertFailure[IllegalMoveException]:
        sm.transition(lastState)(userId, Event.CardClicked(0))

  test("Memory: Won state should contain the id of the player with the most cards won and not another one (2pts)"):
    val lastState = playEntireGame(initState).last.stateWithCardsHidden

    for
      uid <- USER_IDS
      view = sm.project(lastState)(uid).state.assertInstanceOf[StateView.Finished]
    do
      assertEquals(_, StateView.Finished(Set(UID0)))

/// ## Additional tests

  test("Memory: Cards should be randomized (2pts)"):
    assert((0 to 5).map(_ => guessCards(sm.init(USER_IDS))).distinct.size > 1)

  test("Memory: Flipping cards in a different order should give identical results (4pts)"):
    for matching <- List(true, false) do
      var flip1 = flipTwoRandomCards(matching)
      var flip2 = flipTwoCards(initState, UID0, (flip1.idx2, flip1.idx1))

      assertEquals(flip1.isMatch, matching)
      assertEquals(flip2.isMatch, matching)
      assertEquals(
        projectPlayingViews(USER_IDS)(flip1.stateWithCardsHidden),
        projectPlayingViews(USER_IDS)(flip2.stateWithCardsHidden)
      )

  test("Memory: The number of cards should not change from round to round (1pt)") {
    val nCards = boardSize(initState)
    for flip <- playEntireGame(initState) do
      assertEquals(boardSize(flip.stateWithCardsShown), nCards)
      assertEquals(boardSize(flip.stateWithCardsShown), nCards)
  }

  test("Memory: The game should work with different subsets of players (1pt)"):
    for
      n <- 1 to USER_IDS.length
      c <- USER_IDS.combinations(n)
      if c.contains(UID0)
    do
      playEntireGame(sm.init(Seq(UID0)))

/// ## Encoding and decoding

  test("Memory: Different views are not equal (0pt)"):
    val v1 = View(StateView.Finished(Set(UID0)), Map())
    val v2 = View(StateView.Finished(Set(UID1)), Map())
    assertNotEquals(v1, v2)

  test("Memory: Event wire (2pt)"):
    for cardId <- 0 to Short.MaxValue do
      Event.CardClicked(cardId).testEventWire

  test("Memory: View wire (8pts)"):
    for
      n <- 1 to USER_IDS.length
      userIds = USER_IDS.take(n)
      flip <- playEntireGame(sm.init(userIds))
      s <- flip.allStates
      u <- userIds
    do
      sm.project(s)(u).testViewWire
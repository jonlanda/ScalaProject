package logic

class Blackjack {
  private var deck: Deck = _
  private var players: List[Player] = List()
  private var playerHands: Map[Player, List[Card]] = Map()
  private var playerBets: Map[Player, Double] = Map()
  private var dealerHand: List[Card] = List()

  def addPlayer(player: Player): Unit = {
    players = player :: players
  }

  def getPlayers: List[Player] = players

  def start(): Unit = {
    resetDeck()
    dealInitialCards()
  }

  private def resetDeck(): Unit = {
    deck = new Deck()
    playerHands = Map()
    playerBets = Map()
    dealerHand = List()
  }

  def placeBet(player: Player, bet: Double): Boolean = {
    if (bet <= player.balance) {
      player.bet = bet
      playerBets += (player -> bet)
      player.balance -= bet
      true
    } else {
      false
    }
  }

  private def dealInitialCards(): Unit = {
    players.foreach { player =>
      val hand = List(deck.drawCard(), deck.drawCard()).flatten
      playerHands += (player -> hand)
    }
    dealerHand = List(deck.drawCard(), deck.drawCard()).flatten
  }

  def playerTurn(player: Player): String = {
    val hand = playerHands(player)
    if (handValue(hand) > 21) {
      s"${player.name} busts!"
    } else {
      s"${player.name}'s hand: ${hand.mkString(", ")}"
    }
  }

  def hit(player: Player): String = {
    val newCard = deck.drawCard()
    newCard match {
      case Some(card) =>
        val updatedHand = playerHands(player) :+ card
        playerHands += (player -> updatedHand)
        deck.removeCard(card)
        playerTurn(player)
      case None => "No more cards in the deck."
    }
  }

  def stand(player: Player): Unit = {
    // Mark the player as having finished their turn
  }

  def dealerTurn(log: String => Unit): String = {
    while (handValue(dealerHand) < 17 || (handValue(dealerHand) == 17 && containsAce(dealerHand))) {
      val newCard = deck.drawCard().get
      dealerHand :+= newCard
      log(s"Dealer draws: $newCard")
    }
    if (handValue(dealerHand) > 21) {
      "Dealer busts!"
    } else {
      s"Dealer stands with: ${handValue(dealerHand)}"
    }
  }

  def determineWinners(log: String => Unit): List[String] = {
    val dealerTotal = handValue(dealerHand)
    log(s"Dealer's final hand: ${dealerHand.mkString(", ")} with value: $dealerTotal \n")
    val results = players.map { player =>
      val playerTotal = handValue(playerHands(player))
      if (playerTotal > 21) {
        s"${player.name} busts and loses their bet of ${player.bet}"
      } else if (dealerTotal > 21 || playerTotal > dealerTotal) {
        player.balance += 2 * player.bet
        s"${player.name} wins and receives ${2 * player.bet}"
      } else if (playerTotal == dealerTotal) {
        player.balance += player.bet
        s"${player.name} pushes and gets back their bet of ${player.bet}"
      } else {
        s"${player.name} loses their bet of ${player.bet}"
      }
    }
    results
  }

  def getPlayerHand(player: Player): List[Card] = playerHands(player)

  def getAllPlayersHands: Map[Player, List[Card]] = playerHands

  def getDealerHand: List[Card] = dealerHand
  def getPlayerBalance(player: Player): Double = player.balance
  def getPlayerBet(player: Player): Double = player.bet
  def handValue(hand: List[Card]): Int = {
    val values = hand.map {
      case Card(_, "A") => 11
      case Card(_, "K") | Card(_, "Q") | Card(_, "J") => 10
      case Card(_, value) => value.toInt
    }
    val total = values.sum
    if (total > 21 && hand.exists(_.value == "A")) {
      total - 10
    } else {
      total
    }
  }

  private def containsAce(hand: List[Card]): Boolean = {
    hand.exists(_.value == "A")
  }
}

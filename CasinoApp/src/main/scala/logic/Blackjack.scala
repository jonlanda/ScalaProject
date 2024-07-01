package logic

case class Blackjack(
                      deck: Deck,
                      players: List[Player],
                      playerHands: Map[Player, List[Card]],
                      playerBets: Map[Player, Double],
                      dealerHand: List[Card]
                    ) {
  def addPlayer(player: Player): Blackjack = {
    copy(players = player :: players)
  }

  def getPlayers: List[Player] = players

  def start(): Blackjack = {
    val shuffledDeck = deck.shuffle()
    val initialState = Blackjack(shuffledDeck, players, Map(), Map(), List())
    initialState.dealInitialCards()
  }

  private def dealInitialCards(): Blackjack = {
    val (updatedDeck, updatedHands) = players.foldLeft((deck, Map[Player, List[Card]]())) {
      case ((d, hands), player) =>
        val (card1, d1) = d.drawCard()
        val (card2, d2) = d1.drawCard()
        (d2, hands + (player -> List(card1.get, card2.get)))
    }
    val (dealerCard1, deckAfterDealerCard1) = updatedDeck.drawCard()
    val (dealerCard2, finalDeck) = deckAfterDealerCard1.drawCard()
    val initialDealerHand = List(dealerCard1.get, dealerCard2.get)
    copy(deck = finalDeck, playerHands = updatedHands, dealerHand = initialDealerHand)
  }

  def placeBet(player: Player, bet: Double): Either[String, Blackjack] = {
    if (bet <= player.balance) {
      val updatedPlayer = player.copy(balance = player.balance - bet, bet = bet)
      val updatedPlayers = players.map(p => if (p == player) updatedPlayer else p)
      Right(copy(playerBets = playerBets + (player -> bet), players = updatedPlayers))
    } else {
      Left(s"${player.name} has insufficient balance")
    }
  }

  def getDealerHand: List[Card] = dealerHand

  def getPlayerHand(player: Player): List[Card] = playerHands.getOrElse(player, List())

  def hit(player: Player): (Option[Card], Blackjack) = {
    val (card, updatedDeck) = deck.drawCard()
    val updatedHand = card.map(c => playerHands(player) :+ c).getOrElse(playerHands(player))
    (card, copy(deck = updatedDeck, playerHands = playerHands + (player -> updatedHand)))
  }

  def handValue(hand: List[Card]): Int = {
    val values = hand.map(_.value).map {
      case "A" => 11
      case "K" | "Q" | "J" => 10
      case n => n.toInt
    }.sum

    if (values > 21 && hand.exists(_.value == "A")) {
      values - 10
    } else {
      values
    }
  }

  def dealerTurn(log: String => Unit): Blackjack = {
    var updatedDealerHand = dealerHand
    var updatedDeck = deck
    var dealerTotal = handValue(dealerHand)
    while (dealerTotal < 17) {
      val (newCard, newDeck) = updatedDeck.drawCard()
      newCard match {
        case Some(card) =>
          updatedDealerHand :+= card
          updatedDeck = newDeck
          dealerTotal = handValue(updatedDealerHand)
          log(s"\nDealer draws: ${Card.cardToString(card)}\n")
          log(s"Dealer's hand: ${updatedDealerHand.map(Card.cardToString).mkString(", ")} with value: $dealerTotal")
        case None =>
          log("\nNo more cards in the deck.")
      }
    }
    if (dealerTotal > 21) {
      log("\nDealer busts!")
    } else {
      log(s"\nDealer stands with: $dealerTotal")
    }
    copy(deck = updatedDeck, dealerHand = updatedDealerHand)
  }

  def determineWinners(log: String => Unit): (Blackjack, List[String]) = {
    val updatedBlackjack = dealerTurn(log)
    val dealerTotal = handValue(updatedBlackjack.dealerHand)

    val (finalPlayers, results) = updatedBlackjack.players.map { player =>
      val playerTotal = updatedBlackjack.handValue(updatedBlackjack.getPlayerHand(player))
      val updatedPlayer = if (playerTotal > 21) {
        player
      } else if (dealerTotal > 21 || playerTotal > dealerTotal) {
        player.copy(balance = player.balance + 2 * player.bet)
      } else if (playerTotal == dealerTotal) {
        player.copy(balance = player.balance + player.bet)
      } else {
        player
      }
      val resultMessage = if (playerTotal > 21) {
        s"${player.name} busts and loses their bet of ${player.bet}"
      } else if (dealerTotal > 21 || playerTotal > dealerTotal) {
        s"${player.name} wins and receives ${2 * player.bet}"
      } else if (playerTotal == dealerTotal) {
        s"${player.name} pushes and gets back their bet of ${player.bet}"
      } else {
        s"${player.name} loses their bet of ${player.bet}"
      }
      (updatedPlayer, resultMessage)
    }.unzip

    val finalBlackjack = updatedBlackjack.copy(players = finalPlayers)
    (finalBlackjack, results)
  }
}

object Blackjack {
  def apply(): Blackjack = new Blackjack(Deck(), List(), Map(), Map(), List())
}

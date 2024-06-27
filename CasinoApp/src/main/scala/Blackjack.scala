class Blackjack {
  private var deck: Deck = _
  private var players: List[Player] = List()
  private var playerHands: Map[Player, List[Card]] = Map()
  private var playerBets: Map[Player, Double] = Map()
  private var dealerHand: List[Card] = List()

  def addPlayer(player: Player): Unit = {
    players = player :: players
  }

  def start(): Unit = {
    resetDeck()
    placeBets()
    dealInitialCards()
    playerTurns()
    dealerTurn()
    determineWinners()
  }

  private def resetDeck(): Unit = {
    deck = new Deck()
    playerHands = Map()
    playerBets = Map()
    dealerHand = List()
  }

  private def placeBets(): Unit = {
    players.foreach { player =>
      print(s"${player.name} (${player.balance}), place your bet: ")
      val bet = scala.io.StdIn.readDouble()
      if (bet <= player.balance) {
        player.bet = bet
        playerBets += (player -> bet)
        player.balance -= bet
      } else {
        println("Insufficient balance. Bet not placed.")
      }
      Thread.sleep(1000)
    }
  }

  private def dealInitialCards(): Unit = {
    players.foreach { player =>
      val hand = List(deck.drawCard(), deck.drawCard()).flatten
      playerHands += (player -> hand)
      println(s"${player.name}'s hand: ${hand.mkString(", ")}")
      Thread.sleep(1000)
    }
    dealerHand = List(deck.drawCard(), deck.drawCard()).flatten
    println(s"Dealer's visible card: ${dealerHand.head}")
    Thread.sleep(1000)
  }

  private def playerTurns(): Unit = {
    players.foreach { player =>
      var continue = true
      while (continue && handValue(playerHands(player)) < 21) {
        println(s"Do you want to draw a card, ${player.name}? y/n")
        val wantToContinue = scala.io.StdIn.readLine()
        wantToContinue match {
          case "y" =>
            playRound(player)
            val newHand = playerHands(player)
            println(s"${player.name}'s new hand: ${newHand.mkString(", ")}")
            Thread.sleep(1000)
            if (handValue(newHand) > 21) {
              println(s"${player.name} busts!")
              Thread.sleep(1000)
              continue = false
            }
          case "n" => continue = false
          case _ => println("Invalid choice, please try again.")
        }
        Thread.sleep(1000)
      }
    }
  }

  private def dealerTurn(): Unit = {
    println(s"Dealer's hand: ${dealerHand.mkString(", ")}")
    Thread.sleep(1000)
    while (handValue(dealerHand) < 17 || (handValue(dealerHand) == 17 && containsAce(dealerHand))) {
      val newCard = deck.drawCard().get
      dealerHand :+= newCard
      println(s"Dealer draws: $newCard")
      println(s"Dealer's hand: ${dealerHand.mkString(", ")}")
      Thread.sleep(1000)
    }
    if (handValue(dealerHand) > 21) {
      println("Dealer busts!")
    } else {
      println(s"Dealer stands with: ${handValue(dealerHand)}")
    }
    Thread.sleep(1000)
  }

  private def determineWinners(): Unit = {
    val dealerTotal = handValue(dealerHand)
    println(s"Dealer's final hand: ${dealerHand.mkString(", ")} with value: $dealerTotal")
    players.foreach { player =>
      val playerTotal = handValue(playerHands(player))
      println(s"${player.name}'s final hand: ${playerHands(player).mkString(", ")} with value: $playerTotal")
      if (playerTotal > 21) {
        println(s"${player.name} busts and loses their bet of ${player.bet}")
        println("----------------------------------------------------------")
      } else if (dealerTotal > 21 || playerTotal > dealerTotal) {
        println(s"${player.name} wins and receives ${2 * player.bet}")
        println("------------------------------------------------   ")
        player.balance += 2 * player.bet
      } else if (playerTotal == dealerTotal) {
        println(s"${player.name} pushes and gets back their bet of ${player.bet}")
        println("---------------------------------------------------------------")
        player.balance += player.bet
      } else {
        println(s"${player.name} loses their bet of ${player.bet}")
        println("------------------------------------------------")
      }
      Thread.sleep(1000)
    }
  }

  private def playRound(player: Player): Unit = {
    val newCard = deck.drawCard()
    newCard match {
      case Some(card) =>
        val updatedHand = playerHands(player) :+ card
        playerHands += (player -> updatedHand)
        deck.removeCard(card)
      case None => println("No more cards in the deck.")
    }
  }

  private def handValue(hand: List[Card]): Int = {
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

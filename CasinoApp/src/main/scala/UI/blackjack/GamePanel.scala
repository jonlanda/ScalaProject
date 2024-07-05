package UI.blackjack

import logic.{Blackjack, Card}
import scala.swing._
import scala.swing.event._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GamePanel(var blackjack: Blackjack, switchToUserPanel: (Blackjack) => Unit) extends BoxPanel(Orientation.Vertical) {
  val dealerHandLabel = new Label("Dealer's Hand")
  val dealerHandArea = new TextArea {
    rows = 1
    columns = 20
    editable = false
  }

  val currentPlayerLabel = new Label("")
  val currentPlayerHandArea = new TextArea {
    rows = 1
    columns = 20
    editable = false
  }

  val playersList = new ListView[String] {
    visibleRowCount = 10
    fixedCellWidth = 200
  }

  val hitButton = new Button {
    text = "Hit"
  }

  val standButton = new Button {
    text = "Stand"
  }

  val returnButton = new Button {
    text = "Return"
  }

  contents += new BoxPanel(Orientation.Vertical) {
    contents += dealerHandLabel
    contents += dealerHandArea
  }

  contents += new FlowPanel {
    contents += new BoxPanel(Orientation.Vertical) {
      contents += new Label("Players:")
      contents += new ScrollPane(playersList)
    }
    contents += new BoxPanel(Orientation.Vertical) {
      contents += currentPlayerLabel
      contents += currentPlayerHandArea
    }
  }

  contents += new FlowPanel {
    contents += hitButton
    contents += standButton
  }
  contents += returnButton

  listenTo(hitButton, standButton, returnButton)

  var currentPlayerIndex: Int = 0

  def resetGame(updatedBlackjack: Blackjack): Unit = {
    dealerHandArea.text = ""
    currentPlayerLabel.text = ""
    currentPlayerHandArea.text = ""
    println(s"Starting game with initial state: $updatedBlackjack")
    blackjack = updatedBlackjack.start()
    val dealerHand = blackjack.getDealerHand
    dealerHandArea.text = "Dealer's initial hand: " + dealerHand.headOption.map(Card.cardToString).getOrElse("") // Show only the first card
    updatePlayersList()
    currentPlayerIndex = 0
    if (blackjack.getPlayers.nonEmpty) {
      showCurrentPlayerTurn()
    }
  }

  def updatePlayersList(): Unit = {
    playersList.listData = blackjack.getPlayers.map(player => s"${player.name}: ${player.balance} (${player.bet})")
  }

  def showCurrentPlayerTurn(): Unit = {
    if (currentPlayerIndex < blackjack.getPlayers.length) {
      val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
      currentPlayerLabel.text = s"${currentPlayer.name}'s Hand"
      currentPlayerHandArea.text = blackjack.getPlayerHand(currentPlayer).map(Card.cardToString(_)).mkString(", ")
      println(s"Current player: ${currentPlayer.name}, Balance: ${currentPlayer.balance}, Bet: ${currentPlayer.bet}")
    } else {
      dealerTurn()
    }
  }

  def dealerTurn(): Unit = {
    dealerHandArea.text = blackjack.getDealerHand.map(Card.cardToString(_)).mkString(", ")
    val (updatedBlackjack, results) = blackjack.determineWinners(log)
    blackjack = updatedBlackjack
    results.foreach(result => dealerHandArea.append(result + "\n"))
  }

  def playerBusts(): Unit = {
    if (currentPlayerIndex < blackjack.getPlayers.length) {
      val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
      val playerTotal = blackjack.handValue(blackjack.getPlayerHand(currentPlayer))
      currentPlayerHandArea.append(s"\n${currentPlayer.name} busts with $playerTotal!\n")
      updatePlayerListWithScores()
      currentPlayerIndex += 1
      Future {
        Thread.sleep(2000)
        showCurrentPlayerTurn()
      }
    }
  }

  def updatePlayerListWithScores(): Unit = {
    playersList.listData = blackjack.getPlayers.map { player =>
      val playerTotal = blackjack.handValue(blackjack.getPlayerHand(player))
      s"${player.name}: ${playerTotal} (${player.bet})"
    }
  }

  reactions += {
    case ButtonClicked(`hitButton`) =>
      if (currentPlayerIndex < blackjack.getPlayers.length) {
        val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
        val (newCard, updatedBlackjack) = blackjack.hit(currentPlayer)
        blackjack = updatedBlackjack
        newCard.foreach { card =>
          currentPlayerHandArea.append(s", ${Card.cardToString(card)}")
          if (blackjack.handValue(blackjack.getPlayerHand(currentPlayer)) > 21) {
            playerBusts()
          }
        }
      }

    case ButtonClicked(`standButton`) =>
      if (currentPlayerIndex < blackjack.getPlayers.length) {
        val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
        val playerTotal = blackjack.handValue(blackjack.getPlayerHand(currentPlayer))
        currentPlayerHandArea.append(s"\nPlayer stands with $playerTotal\n")
        updatePlayerListWithScores()
        currentPlayerIndex += 1
        showCurrentPlayerTurn()
      }

    case ButtonClicked(`returnButton`) =>
      switchToUserPanel(blackjack)
  }

  def log(message: String): Unit = {
    dealerHandArea.append(message + "\n")
  }
}

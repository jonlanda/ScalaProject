import scala.swing._
import scala.swing.event._
import javax.swing.JPanel
import logic._

import java.awt.CardLayout
import javax.swing.table.DefaultTableModel

object BlackjackUI extends SimpleSwingApplication {
  val blackjack = new Blackjack()

  // Panel names for CardLayout
  val UserPanelName = "UserPanel"
  val BetPanelName = "BetPanel"
  val GamePanelName = "GamePanel"

  // CardLayout for switching panels
  val cardLayout = new CardLayout()
  val cardPanel = new JPanel(cardLayout)

  // Panels for different screens
  val userPanel = new UserPanel
  val betPanel = new BetPanel
  val gamePanel = new GamePanel

  // Add panels to the CardLayout panel
  cardPanel.add(userPanel.peer, UserPanelName)
  cardPanel.add(betPanel.peer, BetPanelName)
  cardPanel.add(gamePanel.peer, GamePanelName)

  def top: Frame = new MainFrame {
    title = "Blackjack"
    preferredSize = new Dimension(800, 600)
    contents = Component.wrap(cardPanel)
  }

  def switchToBetPanel(): Unit = {
    betPanel.updateBetPanel()
    cardLayout.show(cardPanel, BetPanelName)
  }

  def switchToGamePanel(): Unit = {
    cardLayout.show(cardPanel, GamePanelName)
    gamePanel.resetGame()
  }

  def switchToUserPanel(): Unit = {
    cardLayout.show(cardPanel, UserPanelName)
  }

  class UserPanel extends BoxPanel(Orientation.Vertical) {
    val playerNameField = new TextField {
      columns = 20
    }
    val playerBalanceField = new TextField {
      columns = 10
    }

    val tableModel = new DefaultTableModel(Array[Array[AnyRef]](), Array[AnyRef]("Name", "Balance"))
    val playerTable = new Table {
      model = tableModel
      autoResizeMode = Table.AutoResizeMode.Off
    }
    playerTable.peer.getColumnModel.getColumn(0).setPreferredWidth(150)
    playerTable.peer.getColumnModel.getColumn(1).setPreferredWidth(100)

    val addPlayerButton = new Button {
      text = "Add Player"
    }

    val goToBetPanelButton = new Button {
      text = "Go to Bet Panel"
    }

    contents += new FlowPanel {
      contents += new Label("Player Name:")
      contents += playerNameField
      contents += new Label("Balance:")
      contents += playerBalanceField
      contents += addPlayerButton
    }
    contents += goToBetPanelButton
    contents += new ScrollPane(playerTable)

    listenTo(addPlayerButton, goToBetPanelButton)

    reactions += {
      case ButtonClicked(`addPlayerButton`) =>
        val name = playerNameField.text
        val balance = playerBalanceField.text.toDouble
        val player = Player(name, balance)
        blackjack.addPlayer(player)
        updatePlayerList()

      case ButtonClicked(`goToBetPanelButton`) =>
        switchToBetPanel()
    }

    def updatePlayerList(): Unit = {
      tableModel.setRowCount(0) // Clear existing rows
      blackjack.getPlayers.foreach { player =>
        tableModel.addRow(Array[AnyRef](player.name, player.balance.toString))
      }
    }
  }

  class BetPanel extends BoxPanel(Orientation.Vertical) {
    val returnButton = new Button {
      text = "Return to User Panel"
    }
    val startGameButton = new Button {
      text = "Start Game"
    }

    def updateBetPanel(): Unit = {
      contents.clear()
      contents += returnButton
      contents += startGameButton

      blackjack.getPlayers.foreach { player =>
        val betField = new TextField { columns = 10 }
        val placeBetButton = new Button {
          text = s"Place Bet for ${player.name}"
        }
        val betPlacedCheckBox = new CheckBox {
          text = s"${player.name} placed bet"
          enabled = false
        }

        contents += new FlowPanel {
          contents += new Label(s"${player.name} (balance: ${player.balance}): ")
          contents += betField
          contents += placeBetButton
          contents += betPlacedCheckBox
        }

        listenTo(placeBetButton)

        reactions += {
          case ButtonClicked(`placeBetButton`) =>
            val bet = betField.text.toDouble
            if (blackjack.placeBet(player, bet)) {
              betPlacedCheckBox.selected = true
            } else {
              Dialog.showMessage(contents.head, s"${player.name} has insufficient balance", title = "Bet Error")
            }
            userPanel.updatePlayerList()
        }
      }
    }

    listenTo(returnButton, startGameButton)

    reactions += {
      case ButtonClicked(`returnButton`) =>
        switchToUserPanel()

      case ButtonClicked(`startGameButton`) =>
        switchToGamePanel()
    }
  }

  class GamePanel extends BoxPanel(Orientation.Vertical) {
    val messageArea = new TextArea { rows = 10; columns = 40; editable = false }

    val hitButton = new Button {
      text = "Hit"
    }

    val standButton = new Button {
      text = "Stand"
    }

    val returnButton = new Button {
      text = "Return"
    }

    contents += new FlowPanel {
      contents += hitButton
      contents += standButton
      contents += returnButton
    }
    contents += new ScrollPane(messageArea)

    listenTo(hitButton, standButton, returnButton)

    var currentPlayerIndex: Int = 0

    def resetGame(): Unit = {
      messageArea.text = ""
      blackjack.start()
      val dealerHand = blackjack.getDealerHand
      messageArea.append(s"Dealer's visible card: ${cardToString(dealerHand.head)}\n")
      val playerHands = blackjack.getAllPlayersHands
      playerHands.foreach { case (player, hand) =>
        messageArea.append(s"${player.name}'s hand: ${hand.map(cardToString).mkString(", ")}\n")
      }
      currentPlayerIndex = 0
      if (blackjack.getPlayers.nonEmpty) {
        showCurrentPlayerTurn()
      }
    }

    def showCurrentPlayerTurn(): Unit = {
      if (currentPlayerIndex < blackjack.getPlayers.length) {
        val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
        messageArea.append(s"\n${currentPlayer.name}'s turn\n")
      } else {
        dealerTurnAndResults()
      }
    }

    def dealerTurnAndResults(): Unit = {
      messageArea.append(blackjack.dealerTurn(messageArea.append(_)) + "\n")
      val results = blackjack.determineWinners(messageArea.append(_))
      results.foreach(result => messageArea.append(result + "\n"))
    }

    reactions += {
      case ButtonClicked(`hitButton`) =>
        if (currentPlayerIndex < blackjack.getPlayers.length) {
          val currentPlayer = blackjack.getPlayers(currentPlayerIndex)
          val result = blackjack.hit(currentPlayer)
          val newCard = blackjack.getPlayerHand(currentPlayer).last
          messageArea.append(s"${currentPlayer.name} picks up: ${cardToString(newCard)}\n")
          messageArea.append(result + "\n")
          if (blackjack.handValue(blackjack.getPlayerHand(currentPlayer)) > 21) {
            currentPlayerIndex += 1
            showCurrentPlayerTurn()
          }
        }

      case ButtonClicked(`standButton`) =>
        if (currentPlayerIndex < blackjack.getPlayers.length) {
          messageArea.append("Player stands\n")
          currentPlayerIndex += 1
          showCurrentPlayerTurn()
        }

      case ButtonClicked(`returnButton`) =>
        switchToUserPanel()
    }
  }

  def cardToString(card: Card): String = {
    s"${card.value}${card.suit}"
  }
}

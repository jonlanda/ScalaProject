import UI.blackjack.{BetPanel, GamePanel}
import logic._

import java.awt.CardLayout
import javax.swing.JPanel
import javax.swing.table.DefaultTableModel
import scala.swing._
import scala.swing.event._

object BlackjackUI extends SimpleSwingApplication {
  var blackjack = Blackjack()

  // Panel names for CardLayout
  val UserPanelName = "UserPanel"
  val BetPanelName = "BetPanel"
  val GamePanelName = "GamePanel"

  // CardLayout for switching panels
  val cardLayout = new CardLayout()
  val cardPanel = new JPanel(cardLayout)

  // Panels for different screens
  val userPanel = new UserPanel
  val betPanel = new BetPanel(blackjack, switchToUserPanel _, switchToGamePanel, updateUserPanel)
  val gamePanel = new GamePanel(blackjack, switchToUserPanel _)

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
    betPanel.blackjack = blackjack
    betPanel.updateBetPanel()
    cardLayout.show(cardPanel, BetPanelName)
  }

  def switchToGamePanel(updatedBlackjack: Blackjack): Unit = {
    blackjack = updatedBlackjack
    gamePanel.resetGame(blackjack)
    cardLayout.show(cardPanel, GamePanelName)
  }

  def switchToUserPanel(updatedBlackjack: Blackjack): Unit = {
    blackjack = updatedBlackjack
    userPanel.updatePlayerList()
    cardLayout.show(cardPanel, UserPanelName)
  }

  def updateUserPanel(): Unit = {
    userPanel.updatePlayerList()
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
    contents += new ScrollPane(playerTable)
    contents += goToBetPanelButton

    listenTo(addPlayerButton, goToBetPanelButton)

    reactions += {
      case ButtonClicked(`addPlayerButton`) =>
        val name = playerNameField.text
        val balance = playerBalanceField.text.toDouble
        val player = Player(name, balance)
        blackjack = blackjack.addPlayer(player)
        updatePlayerList()

      case ButtonClicked(`goToBetPanelButton`) =>
        switchToBetPanel()
    }

    def updatePlayerList(): Unit = {
      tableModel.setRowCount(0)
      blackjack.getPlayers.foreach { player =>
        tableModel.addRow(Array[AnyRef](player.name, player.balance.toString))
      }
    }
  }
}

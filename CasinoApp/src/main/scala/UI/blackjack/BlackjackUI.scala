import UI.blackjack.{BetPanel, GamePanel}
import logic._

import java.awt.CardLayout
import javax.swing.JPanel
import javax.swing.table.DefaultTableModel
import scala.swing._
import scala.swing.event._

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
  val betPanel = new BetPanel(blackjack, switchToUserPanel, switchToGamePanel, updateUserPanel)
  val gamePanel = new GamePanel(blackjack, switchToUserPanel)

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
}

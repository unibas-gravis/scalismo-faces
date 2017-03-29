/*
 * Copyright University of Basel, Graphics and Vision Research Group
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package scalismo.faces.gui

import java.awt._
import java.awt.event._
import java.io.File
import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

import scalismo.faces.utils.LanguageUtilities.withMutable

import scala.collection.mutable
import scala.language.implicitConversions

object GUIBlock {
  private val openFrames = mutable.Map.empty[String, GUIFrame]

  /** close all named GUIFrames */
  def closeAllFrames(): Unit = {
    openFrames.synchronized {
      openFrames.values.foreach{ frame => runInSwing{frame.dispose()}}
      openFrames.clear()
    }
  }

  /** get the open frame with the title, None if no such frame exists */
  def getFrame(title: String): Option[GUIFrame] = openFrames.synchronized{ openFrames.get(title) }

  /** execute action in Swing Event Dispatch Thread (async) */
  def runInSwing(action: => Unit): Unit = {
    if (SwingUtilities.isEventDispatchThread)
      action
    else
      SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = action
      })
  }

  /** execute action in Swing Event Dispatch Thread (blocks) */
  def runInSwingWait(action: => Unit): Unit = {
    if (SwingUtilities.isEventDispatchThread)
      action
    else
      SwingUtilities.invokeAndWait(new Runnable {
        override def run(): Unit = action
      })
  }

  /** GUI block: can display itself in a GUIFrame */
  implicit class GUIBlock(val component: JComponent) {
    @deprecated("displayIn(String) opens a new frame if there is no frame with the given title", "0.7")
    def displayInNewFrame(title: String): GUIFrame = displayIn(title)

    /** display component in frame with given title - reuse existing frame with title */
    def displayIn(title: String): GUIFrame = {
      val frame = openFrames.synchronized {
        openFrames.getOrElse(title, withMutable(GUIFrame(title)) { f => openFrames.put(title, f) })
      }
      frame.display(component)
      frame
    }

    /** display the component in a given frame */
    def displayIn(frame: GUIFrame): Unit = frame.display(component)

    /** display the component in a JPanel (removes everything inside and replaces with this component) */
    def displayIn(panel: JPanel): Unit = runInSwing {
      panel.synchronized {
        panel.getComponents.foreach(panel.remove)
        panel.add(component)
      }
    }
  }

  implicit class GUIMenu(val menu: JMenuBar) {
    def displayIn(frame: GUIFrame): Unit = frame.showMenu(menu)

    def displayIn(title: String): GUIFrame = {
      val frame = openFrames.synchronized {
        openFrames.getOrElse(title, withMutable(GUIFrame(title)) { f => openFrames.put(title, f) })
      }
      frame.showMenu(menu)
      frame
    }
  }

  /** horizontal container */
  def shelf(components: JComponent*): JPanel = {
    withMutable(new JPanel()) { panel =>
      panel.setLayout(new FlowLayout())
      components.foreach(panel.add(_))
    }
  }

  /** vertical container, center aligned */
  def stack(components: JComponent*): Box = {
    withMutable(new Box(BoxLayout.Y_AXIS)) { b =>
      components.foreach { c =>
        c.setAlignmentX(Component.CENTER_ALIGNMENT)
        b.add(c)
      }
    }
  }

  /** vertical container, full width */
  def fullWidthStack(compenents: JComponent*): JPanel = {
    withMutable(new JPanel)  { panel =>
      panel.setLayout(new GridBagLayout())
      panel.setAlignmentX(SwingConstants.TOP)
      val gbc = new GridBagConstraints()
      gbc.weightx = 1
      gbc.weighty = 1
      gbc.fill = GridBagConstraints.HORIZONTAL
      gbc.gridwidth = GridBagConstraints.REMAINDER
      gbc.anchor = GridBagConstraints.NORTH
      compenents.foreach(panel.add(_, gbc))
    }
  }

  /** arrange components in a grid */
  def grid(cols: Int, rows: Int, component: => JComponent): JPanel = {
    withMutable(new JPanel()) { p =>
      p.setLayout(new GridLayout(cols, rows))
      for (x <- 0 until cols; y <- 0 until rows) p.add(component)
    }
  }

  /** scroll pane: add scroll bars around component */
  def scroll(comp: JComponent) = new JScrollPane(comp)

  /** display a message */
  def alert(msg: String, parent: JComponent): Unit = javax.swing.JOptionPane.showMessageDialog(parent, msg)

  /** display a message */
  def alert(msg: String): Unit = alert(msg, null)

  /** create a button with a listener and a shortcut
    * @param text text to display on button
    * @param action action to be executed on click
    * @param shortcut e.g. "control S"
    */
  def button(text: String, action: => Unit = {}, shortcut: Option[String] = None): JButton = {
    withMutable(new JButton(text)) { b =>
      b.addActionListener(
        withMutable(
          new AbstractAction() {
            override def actionPerformed(e: ActionEvent): Unit = action
          }
        ){ l =>
          if(shortcut.isDefined){
            l.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(shortcut.get))
            b.getActionMap.put(s"${shortcut}Action", l)
            b.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(l.getValue(Action.ACCELERATOR_KEY).asInstanceOf[KeyStroke], s"${shortcut}Action")
            b.setToolTipText(s"Shortcut: ${shortcut.get}")
          }
        }
      )
    }
  }

  /** create a button of an action */
  def button(action: Action): JButton = withMutable(new JButton(action)) { button =>
    button.setHideActionText(true)
  }

  /** create an file chooser for images */
  def chooser(directory: String): FileChooser = new FileChooser(Some(new File(directory)))

  /** toggle button */
  def toggleButton(text: String, selected: Boolean) = new JToggleButton(text, selected)

  /** create a text box */
  def textBox(cols: Int, text: String): JTextField = {
    withMutable(new JTextField()) { t =>
      t.setColumns(cols)
      t.setText(text)
    }
  }

  /** create a simple label element */
  def label(text: String) = new JLabel(text)

  /** create a slider element */
  def slider(min: Int, max: Int, value: Int, changeListener: Int => Unit, orientation: Int = SwingConstants.VERTICAL): JSlider = {
    withMutable(new JSlider(orientation, min, max, value)) { s =>
      s.addChangeListener(new ChangeListener {
        override def stateChanged(e: ChangeEvent): Unit = changeListener(s.getValue)
      })
    }
  }

  /** Swing separator */
  def separator(orientation: Int) = new JSeparator(orientation)

  def separator = new JSeparator()

  /** create container of a preferred size */
  def sizedContainer(width: Int, height: Int, component: JComponent): JPanel = {
    withMutable(new JPanel()) { panel =>
      panel.setPreferredSize(new Dimension(width, height))
      panel.add(component)
    }
  }

  // toolbar
  /** toolbar items: can be separators or entries (usually buttons) */
  sealed trait ToolbarItem
  case object ToolbarSeparator extends ToolbarItem
  case class ToolbarAction(action: Action) extends ToolbarItem
  case class ToolbarEntry(entry: JComponent) extends ToolbarItem

  def toolbar(name: String, elements: ToolbarItem*): JToolBar = {
    withMutable(new JToolBar(name)){ tb =>
      elements.foreach{
        case ToolbarSeparator => tb.addSeparator()
        case ToolbarEntry(component) => tb.add(component)
        case ToolbarAction(action) => tb.add(action)
      }
    }
  }

  def toolbarSeparator: ToolbarItem = ToolbarSeparator

  def toolbarItem(action: Action) = ToolbarAction(action)

  def toolbarItem(component: JComponent) = ToolbarEntry(component)

  /** create an empty GUI element (JPanel) */
  def empty: JPanel = new JPanel()

  /** create a border layout container (with 5 zones, auto scaling center) */
  def borderLayout(top: Component = empty,
                   center: Component = empty,
                   left: Component = empty,
                   right: Component = empty,
                   bottom: Component = empty): JPanel = {
    withMutable(new JPanel()) { panel =>
      panel.setLayout(new BorderLayout())
      panel.add(top, BorderLayout.PAGE_START)
      panel.add(center, BorderLayout.CENTER)
      panel.add(left, BorderLayout.LINE_START)
      panel.add(right, BorderLayout.LINE_END)
      panel.add(bottom, BorderLayout.PAGE_END)
    }
  }

  /** horizontal split pane */
  def horizontalSplitter(left: JComponent, right: JComponent, resizeWeight: Double = 0.5): JSplitPane = {
    withMutable(new JSplitPane()) { p =>
      p.setOrientation(JSplitPane.HORIZONTAL_SPLIT)
      p.setLeftComponent(left)
      p.setRightComponent(right)
      p.setResizeWeight(resizeWeight)
    }
  }

  /** vertical split pane */
  def verticalSplitter(top: JComponent, bottom: JComponent, resizeWeight: Double = 0.5): JSplitPane = {
    withMutable(new JSplitPane()) { p =>
      p.setOrientation(JSplitPane.VERTICAL_SPLIT)
      p.setTopComponent(top)
      p.setBottomComponent(bottom)
      p.setResizeWeight(resizeWeight)
    }
  }

  def horizontalSeparator: JSeparator = new JSeparator(SwingConstants.HORIZONTAL)

  def verticalSeparator: JSeparator = new JSeparator(SwingConstants.VERTICAL)

  /** create an action (action object to be used in menus and buttons) */
  def action(text: String = "",
             tooltip: String = "",
             icon: Icon = null,
             accelerator: KeyStroke = null,
             action: => Unit = {}): Action = {
    withMutable(new AbstractAction(text, icon) {
      override def actionPerformed(e: ActionEvent): Unit = action
    }) { a =>
      for (acc <- Option(accelerator)) a.putValue(Action.ACCELERATOR_KEY, acc)
      if (tooltip.nonEmpty)
        a.putValue(Action.SHORT_DESCRIPTION, tooltip)
      else if (text.nonEmpty)
        a.putValue(Action.SHORT_DESCRIPTION, text)
    }
  }

  /** create an action (action object to be used in menus and buttons) */
  def action(name: String, action: => Unit): Action = {
    withMutable(new AbstractAction(name) {
      override def actionPerformed(e: ActionEvent): Unit = action
    }){ action =>
      action.putValue(Action.SHORT_DESCRIPTION, name)
    }
  }

  // ** menu **

  // need an own menu item type to wrap separators, items and sub menus all in one type
  sealed trait MenuItem
  case object MenuSeparator extends MenuItem
  case class  MenuEntry(item: JMenuItem) extends MenuItem
  case class  Menu(menu: JMenu) extends MenuItem

  implicit def menu2JMenu(menu: Menu): JMenu = menu.menu
  implicit def menuEntry2JMenuItem(menuEntry: MenuEntry): JMenuItem = menuEntry.item
  implicit def swingMenu2Menu(jMenu: JMenu): Menu = Menu(jMenu)
  implicit def swingMenuItem2MenuEntry(jMenuItem: JMenuItem): MenuEntry = MenuEntry(jMenuItem)

  def menuItem(action: Action): MenuEntry = {
    MenuEntry(new JMenuItem(action))
  }

  def menuItem(name: String, action: => Unit): MenuEntry = {
    val item = new JMenuItem(name)
    item.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = action
    })
    MenuEntry(item)
  }

  def menuItem(name: String, action: => Unit, accelerator: KeyStroke, toolTip: String = ""): MenuEntry = {
    val item = new JMenuItem(name)
    item.setAccelerator(accelerator)
    item.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = action
    })
    MenuEntry(item)
  }

  def menuItemRadio(name: String, action: => Unit, accelerator: KeyStroke, toolTip: String = ""): MenuEntry = {
    val item = new JRadioButtonMenuItem(name)
    item.setAccelerator(accelerator)
    item.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = action
    })
    MenuEntry(item)
  }

  def menuItemRadio(action: Action): MenuEntry = {
    MenuEntry(new JRadioButtonMenuItem(action))
  }

  /** add a separator to a menu */
  def menuSeparator: MenuItem = MenuSeparator

  /** create a menu consisting of menu items (entry, separator or sub menu) */
  def menu(name: String, items: MenuItem*): Menu = {
    val menu = new JMenu(name)
    items.foreach{
      case MenuEntry(item) => menu.add(item)
      case Menu(m) => menu.add(m)
      case MenuSeparator => menu.addSeparator()
    }
    Menu(menu)
  }

  /** create a menu bar of multiple menus */
  def menuBar(menus: Menu*): JMenuBar = {
    withMutable(new JMenuBar()) { mb =>
      menus.foreach(menu => mb.add(menu.menu))
    }
  }
}






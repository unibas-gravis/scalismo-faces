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

import java.awt.BorderLayout
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.{JComponent, JFrame, JMenuBar}

import scalismo.faces.utils.LanguageUtilities.withMutable

/** simple master frame to display arbitrary swing JComponents, use with GUIBlock */
class GUIFrame(title: String) extends JFrame(title) {
  // dispose frame after closing
  addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent): Unit = dispose()
  })

  /** display a GUI component */
  def display(pane: JComponent): Unit = synchronized {
    setLayout(new BorderLayout())
    getContentPane.removeAll()
    add(pane, BorderLayout.CENTER)
    pack()
    setVisible(true)
  }

  def showMenu(menu: JMenuBar): Unit = synchronized { setJMenuBar(menu) }

}

object GUIFrame {
  def apply(title: String): GUIFrame = new GUIFrame(title)
  def apply(title: String, component: JComponent): GUIFrame = withMutable(new GUIFrame(title)) { f =>
    f.display(component)
  }
}

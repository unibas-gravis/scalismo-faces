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

import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter

/** file chooser dialog with a possible extension filter */
class FileChooser(directory: Option[File] = None, fileFilter: Option[FileNameExtensionFilter] = None)
    extends JFileChooser {
  fileFilter.foreach(setFileFilter)
  directory.foreach(setCurrentDirectory)

  /** execute action on chosen file */
  def onFileChosen(actionListener: (ActionEvent) => Unit): Unit = {
    addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = actionListener(e)
    })
  }
}

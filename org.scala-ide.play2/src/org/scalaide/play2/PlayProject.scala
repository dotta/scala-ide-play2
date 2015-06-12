package org.scalaide.play2

import java.io.File
import org.scalaide.core.compiler.IScalaPresentationCompiler
import org.scalaide.core.IScalaProject
import org.eclipse.core.resources.IFile
import org.scalaide.play2.templateeditor.TemplateCompilationUnit
import org.eclipse.ui.preferences.ScopedPreferenceStore
import org.eclipse.core.resources.ProjectScope
import org.eclipse.jface.preference.IPreferenceStore
import org.scalaide.play2.properties.PlayPreferences
import scala.collection.mutable
import org.eclipse.jface.util.PropertyChangeEvent

class PlayProject private (scalaProject: IScalaProject) {
  @volatile private var _playSupport: Play = Play(Play.DefaultVersion)

  def playSupport: Play = _playSupport

  lazy val projectPrefStore = generateScopedPreferenceStore

  /** Return a new project-scoped preference store for this project. */
  def generateScopedPreferenceStore: IPreferenceStore = new ScopedPreferenceStore(new ProjectScope(scalaProject.underlying), PlayPlugin.PluginId)

  /** Tries to load the scala template files */
  private def initialize() {
    initializePreferences()
    initializeTemplates()
  }

  private def initializePreferences(): Unit = {
    import org.scalaide.util.eclipse.SWTUtils.fnToPropertyChangeListener
    projectPrefStore.addPropertyChangeListener((event: PropertyChangeEvent) => event.getProperty() match {
      case PlayPreferences.PlayVersion =>
        val value = event.getNewValue().toString()
        _playSupport = Play(value)
        import org.eclipse.core.runtime.IStatus
        PlayPlugin.log(IStatus.OK, s"Value of ${PlayPreferences.PlayVersion} is ${value}")
        // do something with the new value
      case PlayPreferences.TemplateImports =>
        val value = event.getNewValue().toString()
        import org.eclipse.core.runtime.IStatus
        PlayPlugin.log(IStatus.OK, s"Value of ${PlayPreferences.TemplateImports} is ${value}")
      case other =>
        import org.eclipse.core.runtime.IStatus
        PlayPlugin.log(IStatus.OK, s"Not interested on $other")
    });
  }

  private def initializeTemplates(): Unit = {
    for { 
      r <- scalaProject.underlying.members()
      if r.isInstanceOf[IFile] && r.getFullPath().toString().endsWith("." + PlayPlugin.TemplateExtension)
    } {
      // FIXME: I wonder what happens is the presentation compiler is restarted. 
      //        Will the template compilation units still be loaded?
      //        If not, then why do we actually need to load them in the first place?
      val unit = TemplateCompilationUnit(r.asInstanceOf[IFile], false)
      unit.initialReconcile()
    }
  }
  
  /** FIXME: This method should probably not exist.
   *         Template files can be anywhere
   *
   *  @return the absolute location of the project directory
   */
  private[play2] lazy val sourceDir = scalaProject.underlying.getLocation().toFile()
}

object PlayProject {
  // FIXME: This is a source of memory leaks. We should remove the project from the map if it's deleted.
  private val projects = (new mutable.HashMap) withDefault {(scalaProject: IScalaProject) => new PlayProject(scalaProject)}

  def apply(scalaProject: IScalaProject): PlayProject = {
    val project = projects(scalaProject)
    project.initialize()
    project
  }
}
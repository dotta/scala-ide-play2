package org.scalaide.play2

import org.scalaide.core.IScalaPlugin
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Status
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.osgi.framework.BundleContext
import org.eclipse.jface.resource.ImageRegistry
import org.scalaide.play2.util.Images

object PlayPlugin {
  @volatile
  private var plugin: PlayPlugin = _

  final val PluginId = "org.scala-ide.play2"
  final val RouteFormatterMarginId = PluginId + ".routeeditor.margin"
  final val RouteFormatterFormatOnSaveId = PluginId + ".routeeditor.formatonsave"
  final val TemplateExtension = "scala.html"

  /** Return the current plugin instace */
  def instance(): PlayPlugin = plugin

  def getImageDescriptor(path: String): ImageDescriptor =
    AbstractUIPlugin.imageDescriptorFromPlugin(PluginId, path)

  def log(status: Int, msg: String, ex: Throwable = null): Unit =
    plugin.getLog.log(new Status(status, plugin.getBundle().getSymbolicName(), msg, ex))

  def isPlayProject(project: IProject): Boolean = asPlayProject(project).isDefined

  def asPlayProject(project: IProject): Option[PlayProject] = {
    for {
      scalaPlugin <- Option(IScalaPlugin())
      scalaProject <- scalaPlugin.asScalaProject(project)
    } yield PlayProject(scalaProject)
  }
}

class PlayPlugin extends AbstractUIPlugin {
  override def start(context: BundleContext): Unit = {
    PlayPlugin.plugin = this
    super.start(context)
    initializeProjects()
  }

  private def initializeProjects(): Unit = {
    // FIXME: All Scala projects are paying a penalty if the Play2 support is installed.
    //        I don't like it, but I'm not sure how to fix this. Maybe we should add a 
    //        Play Nature?
    //        Also, how is the `Play2PropertyTester` used/useful?
    for {
      project <- ResourcesPlugin.getWorkspace.getRoot.getProjects
      if project.isOpen
    } PlayPlugin.asPlayProject(project)
  }

  override def stop(context: BundleContext): Unit = {
    PlayPlugin.plugin = null
    super.stop(context)
  }

  override def initializeImageRegistry(reg: ImageRegistry) {
    reg.put(Images.ROUTES_ICON, Images.ROUTES_ICON_DESCRIPTOR)
    reg.put(Images.HTTP_METHODS_ICON, Images.HTTP_METHODS_ICON_DESCRIPTOR)
    reg.put(Images.URL_ICON, Images.URL_ICON_DESCRIPTOR)
  }
}

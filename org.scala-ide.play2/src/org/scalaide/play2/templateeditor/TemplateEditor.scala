package org.scalaide.play2.templateeditor

import scala.collection.JavaConverters
import org.eclipse.jdt.core.compiler.IProblem
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitDocumentProvider.ProblemAnnotation
import org.eclipse.jface.text.Position
import org.eclipse.jface.text.source.IAnnotationModel
import org.eclipse.jface.text.source.IAnnotationModelExtension
import org.eclipse.jface.text.source.IAnnotationModelExtension2
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.jface.util.PropertyChangeEvent
import org.eclipse.ui.editors.text.EditorsUI
import org.eclipse.ui.texteditor.ChainedPreferenceStore
import org.scalaide.play2.PlayPlugin
import org.scalaide.ui.editor.SourceCodeEditor
import org.scalaide.ui.editor.CompilationUnitProvider
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.ui.editors.text.TextEditor
import org.eclipse.jface.text.source.IVerticalRuler
import org.eclipse.swt.widgets.Composite
import org.scalaide.play2.util.StoredEditorUtils

trait AbstractTemplateEditor extends SourceCodeEditor { self: TextEditor =>

  override protected type UnderlyingCompilationUnit = TemplateCompilationUnit

  override val compilationUnitProvider: CompilationUnitProvider[UnderlyingCompilationUnit] = new TemplateCompilationUnitProvider(false)
}

class TemplateEditor extends TextEditor with AbstractTemplateEditor {

  override protected lazy val preferenceStore: IPreferenceStore = new ChainedPreferenceStore(Array((EditorsUI.getPreferenceStore()), PlayPlugin.instance().getPreferenceStore()))
  private val sourceViewConfiguration = new TemplateConfiguration(preferenceStore, this)
  private val documentProvider = new TemplateDocumentProvider()

  setSourceViewerConfiguration(sourceViewConfiguration);
  setPreferenceStore(preferenceStore)
  setDocumentProvider(documentProvider);

  override def handlePreferenceStoreChanged(event: PropertyChangeEvent) = {
    sourceViewConfiguration.propertyChange(event)
    super.handlePreferenceStoreChanged(event)
  }

  override def affectsTextPresentation(event: PropertyChangeEvent): Boolean = {
    // TODO: more precise filtering
    true
  }

  override def editorSaved() = {
    super.editorSaved()
    sourceViewConfiguration.strategy.reconcile(null)
  }
  override def createSourceViewer(parent: Composite, verticalRuler: IVerticalRuler, styles: Int): ISourceViewer = {
    val sourceViewer = super.createSourceViewer(parent, verticalRuler, styles)
    StoredEditorUtils.storeEditorInViewer(sourceViewer, this)
    sourceViewer
  }
}

object TemplateEditor {
  /** The annotation types shown when hovering on the left-side ruler (or in the status bar). */
  val annotationsShownInHover = Set(
    "org.eclipse.jdt.ui.error", "org.eclipse.jdt.ui.warning", "org.eclipse.jdt.ui.info")
}
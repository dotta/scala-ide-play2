package org.scalaide.play2.routeeditor.properties

import org.eclipse.jface.preference.FieldEditorPreferencePage
import org.eclipse.jface.preference.IntegerFieldEditor
import org.eclipse.ui.IWorkbench
import org.eclipse.ui.IWorkbenchPreferencePage
import org.scalaide.play2.PlayPlugin

class RouteFormatterPreferencePage extends FieldEditorPreferencePage with IWorkbenchPreferencePage {

  override protected def init(workbench: IWorkbench): Unit = {
    setPreferenceStore(PlayPlugin.instance().getPreferenceStore());
    setDescription("Play routes preference page");
  }

  override def createFieldEditors() {
    val marginField = new IntegerFieldEditor(PlayPlugin.RouteFormatterMarginId, "Number of spaces between columns", getFieldEditorParent)
    marginField.setValidRange(1, 10)
    addField(marginField)
    
    val formatOnSaveToggle = new org.eclipse.jface.preference.BooleanFieldEditor(PlayPlugin.RouteFormatterFormatOnSaveId, "Format on save", getFieldEditorParent)
    addField(formatOnSaveToggle);
  }
}
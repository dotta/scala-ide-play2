package org.scalaide.play2.properties

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer
import org.scalaide.play2.PlayPlugin

class PreferenceInitializer extends AbstractPreferenceInitializer {
  override def initializeDefaultPreferences(): Unit = {
    PlayPlugin.instance().getPreferenceStore().setDefault(PlayPreferences.TemplateImports, "")
  }
}
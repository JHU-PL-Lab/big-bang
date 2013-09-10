package edu.jhu.cs.bigbang.eclipse.editors;

import org.eclipse.ui.editors.text.TextEditor;

/**
 * A BigBang editor
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class Editor extends TextEditor {

	public Editor() {
		super();
		setSourceViewerConfiguration(new Configuration());
	}

}

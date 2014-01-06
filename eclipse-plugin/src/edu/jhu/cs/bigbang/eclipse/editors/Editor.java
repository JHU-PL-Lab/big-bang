package edu.jhu.cs.bigbang.eclipse.editors;

import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.ui.editors.text.TextEditor;

/**
 * A BigBang file editor
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class Editor extends TextEditor {

	/** A configuration of this editor */
	private SourceViewerConfiguration configuration;

	public Editor() {
		super();
		configuration = new Configuration();
		setSourceViewerConfiguration(configuration);
	}

}

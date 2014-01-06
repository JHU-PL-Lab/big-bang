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
	/** A semantic hilighter */
	private SemanticHighlighter semanticHL;
	/** A separate thread to perform hightlighting*/
	private Thread hilightThread;
	
	public Editor() {
		super();
		configuration = new Configuration();
		setSourceViewerConfiguration(configuration);
		semanticHL = new SemanticHighlighter(this);
		hilightThread = new Thread(semanticHL);
		hilightThread.start();		
	}

	@Override
	public void dispose() {
		semanticHL.kill();
		super.dispose();
	}
	
	
}

package edu.jhu.cs.bigbang.eclipse.editors;

import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class SemanticHighlighter implements Runnable {

	public static int DEFAULT_DELAY = 1000;

	private Editor editor;
	private boolean running;
	private int delay;

	public SemanticHighlighter(Editor editor) {
		this.editor = editor;
		this.running = false;
		this.delay = DEFAULT_DELAY;
	}

	public void highlightFirstWord() {
		ITextOperationTarget target = (ITextOperationTarget) editor
				.getAdapter(ITextOperationTarget.class);
		if (target instanceof ITextViewer) {
			final ITextViewer textViewer = (ITextViewer) target;
			final Color redColor = new Color(Display.getCurrent(), new RGB(255, 0, 0));
			Display.getDefault().asyncExec(new Runnable() {
	               public void run() {
	            	   textViewer.setTextColor(redColor, 0, 1, true);
	               }
			});
		}	
	}

	@Override
	public void run() {
		running = true;
		while (running) {
			highlightFirstWord();
			try {
				Thread.sleep(delay);
			} catch (InterruptedException e) {
				System.err.println("Error while running semantichighlighter");
				e.printStackTrace();
			}
		}
	}

	public void kill() {
		running = false;
	}

}

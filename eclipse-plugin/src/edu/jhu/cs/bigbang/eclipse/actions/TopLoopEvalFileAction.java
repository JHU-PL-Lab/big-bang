package edu.jhu.cs.bigbang.eclipse.actions;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.editors.text.TextEditor;


/**
 * This class defines an action to evaluate the content
 * of the current file in the top loop (All lines).
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class TopLoopEvalFileAction extends TopLoopEvalAction {

	@Override
	protected String getTargetString(TextEditor editor, TextSelection selection) {
		IEditorInput input = editor.getEditorInput();
		
		IDocument document = editor.getDocumentProvider().getDocument(input);
		
		String strSelected = "";
		try {
			// Grab the text from the whole file
			strSelected = document.get(0, document.getLength());
		} catch (BadLocationException e1) {
			System.err.println("Error while grabbing the text in the editor.");
		}
		strSelected = strSelected.trim();
		return strSelected;
	}

}

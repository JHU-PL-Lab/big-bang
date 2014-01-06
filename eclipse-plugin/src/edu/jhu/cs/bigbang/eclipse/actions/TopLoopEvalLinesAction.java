package edu.jhu.cs.bigbang.eclipse.actions;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.editors.text.TextEditor;


/**
 * This class defines an action to evaluate the content
 * of the current file in the selected lines.
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class TopLoopEvalLinesAction extends TopLoopEvalAction {

	@Override
	protected String getTargetString(TextEditor editor, TextSelection selection) {	
		IEditorInput input = editor.getEditorInput();
		IDocument document = editor.getDocumentProvider().getDocument(input);
		String strSelected = "";
		// Grab the text from the selected single line
		try {
			strSelected = document.get(selection.getOffset(),
					selection.getLength());
		} catch (BadLocationException e1) {
			System.err.println("Error while grabbing the text in the editor.");
		}
		strSelected = strSelected.trim();
		//  If the content is empty, may be the users use the region
		// and not the selected line.
		if (strSelected.equals("")) {
			// This means the selected region is just empty space
			if (selection.getLength() != 0)
				return "";
			// Get the content from the whole selected region
			try {
				IRegion line = document.getLineInformationOfOffset(selection
						.getOffset());
				strSelected = document.get(line.getOffset(), line.getLength());
				strSelected = strSelected.trim();
			} catch (BadLocationException e1) {
				System.err
						.println("Error while grabbing the text in the editor.");
			}
		}
		return strSelected;
	}

}

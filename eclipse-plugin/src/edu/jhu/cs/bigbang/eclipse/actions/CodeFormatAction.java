package edu.jhu.cs.bigbang.eclipse.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import edu.jhu.cs.bigbang.eclipse.editors.helper.CodeFormatter;

/**
 * An action to format the code in the editor.
 * This action is binded in plugin.xml
 * The current sequence is 'F8'
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class CodeFormatAction implements IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow window;
	private CodeFormatter codeFormatter;
	
	public CodeFormatAction() {
		super();
		codeFormatter = new CodeFormatter();
	}
	
	@Override
	public void run(IAction action) {
		IWorkbenchPage page = window.getActivePage();
		// We only do the job if we are on a page
		if (page != null) {
			IEditorPart editorPart = page.getActiveEditor();
			// There must be a source editor in the page too.
			if (editorPart != null) {
				ITextOperationTarget target = (ITextOperationTarget) editorPart
						.getAdapter(ITextOperationTarget.class);
				// Only text can be formatted
				if (target instanceof ITextViewer) {
					ITextViewer textViewer = (ITextViewer) target;
					IDocument d = textViewer.getDocument();
					try {
						codeFormatter.format(d, d.getPartition(0));
					} catch (BadLocationException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}

	@Override
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

	@Override
	public void selectionChanged(IAction arg0, ISelection arg1) {
		// Unimplemented
	}

	@Override
	public void dispose() {
		// Unimplemented
	}

}

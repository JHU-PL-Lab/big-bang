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

public class CodeFormatAction implements IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow window;

	@Override
	public void run(IAction arg0) {
		IWorkbenchPage page = window.getActivePage();
		// We only do the job if there is a page exists.
		if (page != null) {
			IEditorPart editorPart = page.getActiveEditor();
			// There must be a source editor in the page too.
			if (editorPart != null) {
				ITextOperationTarget target = (ITextOperationTarget) editorPart
						.getAdapter(ITextOperationTarget.class);
				if (target instanceof ITextViewer) {
					ITextViewer textViewer = (ITextViewer) target;
					CodeFormatter c = new CodeFormatter();
					IDocument d = textViewer.getDocument();
					try {
						c.format(d, d.getPartition(0));
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

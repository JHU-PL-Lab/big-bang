package edu.jhu.cs.bigbang.eclipse.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.TextEditor;

import edu.jhu.cs.bigbang.eclipse.toploop.TopLoop;
import edu.jhu.cs.bigbang.eclipse.toploop.TopLoopView;

/**
 * The base class for a quick evaluation in the top loop.
 * The children must define how they want to grab text
 * to evaluate on their own.
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public abstract class TopLoopEvalAction implements
		IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow window;

	/**
	 * Get the target string from the editor.
	 * @param editor The focused editor
	 * @param selection The selection line
	 * @return The target string
	 */
	protected abstract String getTargetString(TextEditor editor,
			TextSelection selection);

	/**
	 * Call a toploop to eval the given string
	 * @param lines string to be evaled
	 */
	public void evalStringInTopLoop(String lines) {
		// We split them in to an array of lines
		// and pass them one by one to the TopLoop
		lines = lines.trim();
		String[] eval = lines.split("\n+");
		for (String s : eval) 
			TopLoop.getInstance().eval(s);
	}
	
	@Override
	public final void run(IAction action) {
		IWorkbenchPage page = window.getActivePage();
		// We only do the job if there is a page exists.
		if (page != null) {
			IEditorPart editorPart = page.getActiveEditor();
			// There must be a source editor in the page too.
			if (editorPart != null) {
				TopLoopView.forceShowTopLoopView();
				// Get the selection part of the editor
				TextEditor editor = (TextEditor) editorPart;
				ISelection sel = editor.getSelectionProvider().getSelection();
				if (sel instanceof TextSelection) {
					String strSelected = getTargetString(editor,
							(TextSelection) sel);
					// Nothing to be done
					if(!strSelected.equals(""))
						evalStringInTopLoop(strSelected);
				}
			} else {
				MessageDialog.openInformation(window.getShell(),
						"Bigbang Plugin",
						"There is no Bigbang file to eval in the top loop.");
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

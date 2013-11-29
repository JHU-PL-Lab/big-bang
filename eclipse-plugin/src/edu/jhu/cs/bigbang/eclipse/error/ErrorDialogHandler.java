package edu.jhu.cs.bigbang.eclipse.error;

import org.eclipse.jface.dialogs.MessageDialog;

/**
 * This class is responsible for showing an error message diable.
 * 
 * @author Keeratipong Ukachoke <geeskeiup@gmail.com>
 *
 */
public class ErrorDialogHandler {

	public static void showIntepreterNotFoundError() {
		MessageDialog
				.openError(
						null,
						"Interpreter Error",
						"Can't create a communication with an interpreter. " +
						"Please check if the interpreter path is correctly provided.");
	}

}

package edu.jhu.cs.bigbang.eclipse.editors.helper;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ContentAssistant;

/**
 * An assistant for handling code completion. 
 * Normally, this will be used by Configuration class.
 * 
 * @author Keeratipong Ukachoke <geeskeiup@gmail.com>
 *
 */
public class CodeAssistant extends ContentAssistant {

	/** The delay in millisecods of how fast the proposal will pop up **/
	public static int ACTIVATION_DELAY = 100;

	public CodeAssistant() {
		setContentAssistProcessor(new CodeAssistantProcessor(),
				IDocument.DEFAULT_CONTENT_TYPE);
		enableAutoActivation(true);
		setAutoActivationDelay(ACTIVATION_DELAY);
		setProposalPopupOrientation(CONTEXT_INFO_BELOW);
		setContextInformationPopupOrientation(CONTEXT_INFO_BELOW);
	}

}

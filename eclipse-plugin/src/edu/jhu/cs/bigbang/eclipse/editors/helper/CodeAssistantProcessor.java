package edu.jhu.cs.bigbang.eclipse.editors.helper;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;

/**
 * This is the backbone of the code completion process. 
 * It must be used by CodeAssistant.
 * There are also many other methods that we are currently not interested.
 * 
 * @author Keeratipong Ukachoke <geeskeiup@gmail.com>
 *
 */
public class CodeAssistantProcessor implements IContentAssistProcessor {

	@Override
	public ICompletionProposal[] computeCompletionProposals(
			ITextViewer textViewer, int offset) {
		
		///////////////////////////////////////////////////
		// Note - 11/11/2013
		// This is a dummy code just for demonstration.
		// Return a list of proposals 1- 5 back
		ICompletionProposal[] result = new ICompletionProposal[5];
		for (int i = 0; i < result.length; i++) {
			String s = "Proposal " + i;
			result[i] = new CompletionProposal(s, offset, 0, 1);
		}
		return result;
		// End
		////////////////////////////////////////////////////
	}

	@Override
	public IContextInformation[] computeContextInformation(ITextViewer arg0,
			int arg1) {
		return null;
	}

	@Override
	public char[] getCompletionProposalAutoActivationCharacters() {
		return null;
	}

	@Override
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	@Override
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	@Override
	public String getErrorMessage() {
		return null;
	}

}

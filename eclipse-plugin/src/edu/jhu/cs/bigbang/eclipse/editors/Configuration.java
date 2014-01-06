package edu.jhu.cs.bigbang.eclipse.editors;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.formatter.IContentFormatter;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;

import edu.jhu.cs.bigbang.eclipse.editors.helper.CodeAssistant;
import edu.jhu.cs.bigbang.eclipse.editors.helper.CodeFormatter;

/**
 * A configuration for BigBang editor. This is where we 
 * set up the color highliging and code completion modules.
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
class Configuration extends SourceViewerConfiguration {

	@Override
	public IPresentationReconciler getPresentationReconciler(
			ISourceViewer sourceViewer) {
		PresentationReconciler pr = new PresentationReconciler();
		DefaultDamagerRepairer ddr = new DefaultDamagerRepairer(
				new ColorScanner());
		pr.setRepairer(ddr, IDocument.DEFAULT_CONTENT_TYPE);
		pr.setDamager(ddr, IDocument.DEFAULT_CONTENT_TYPE);
		return pr;
	}

	@Override
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		CodeAssistant codeAssistant = new CodeAssistant();
		return codeAssistant;
	}
	
	@Override
	public IContentFormatter getContentFormatter(ISourceViewer sourceViewer) {
		CodeFormatter codeFormatter = new CodeFormatter();
		return codeFormatter;
	}

}

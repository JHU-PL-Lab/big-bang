package edu.jhu.cs.bigbang.eclipse.editors.helper;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.formatter.ContentFormatter;

public class CodeFormatter extends ContentFormatter{

	public CodeFormatter() {
		setFormattingStrategy(new CodeFormattingStrategy(), IDocument.DEFAULT_CONTENT_TYPE);
	}
	
}

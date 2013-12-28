package edu.jhu.cs.bigbang.communicator.fromHS;

public class TextSource extends SourceLocation{
	
	private SourceDocument textSourceDocument;
	private int textSourceLineNo;
	private int textSourceCoNo;

	public SourceDocument getSourceDoc() {
		return textSourceDocument;
	}

	public void setSourceDoc(SourceDocument sourceDoc) {
		this.textSourceDocument = sourceDoc;
	}

	public int getTextSourceLineNo() {
		return textSourceLineNo;
	}

	public void setTextSourceLineNo(int textSourceLineNo) {
		this.textSourceLineNo = textSourceLineNo;
	}

	public int getTextSourceCoNo() {
		return textSourceCoNo;
	}

	public void setTextSourceCoNo(int textSourceCoNo) {
		this.textSourceCoNo = textSourceCoNo;
	}

	public TextSource(SourceDocument sourceDoc, int textSourceLineNo,
			int textSourceCoNo) {
		this.textSourceDocument = sourceDoc;
		this.textSourceLineNo = textSourceLineNo;
		this.textSourceCoNo = textSourceCoNo;
	}
	
	public String toString() {
		return " " + textSourceDocument + " @ " + this.textSourceLineNo + " - " + this.textSourceCoNo;
	}
	
}

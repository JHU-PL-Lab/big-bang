package edu.jhu.cs.bigbang.communicator.fromHS;

public class SourceDocument {
	
	private String docType;

	public String getDocType() {
		return docType;
	}

	public void setDocType(String docType) {
		this.docType = docType;
	}

	public SourceDocument(String docType) {
		super();
		this.docType = docType;
	}
	
	public String toString() {
		return "Unknown";
	}
	
 }

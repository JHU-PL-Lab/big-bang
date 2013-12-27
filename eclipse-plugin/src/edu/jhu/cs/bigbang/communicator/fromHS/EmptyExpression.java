package edu.jhu.cs.bigbang.communicator.fromHS;

public class EmptyExpression extends IllFormedness{
	
	private String content;

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public EmptyExpression(String content) {
		super();
		this.content = content;
	}
	
	public String toString() {
    	return content;
    }
}

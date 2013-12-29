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
	
	@Override
	public String toString() {
    	return content;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(!(obj instanceof EmptyExpression)) return false;
		EmptyExpression emptyExpressionObj = (EmptyExpression) obj;
		return (this.content.equals(emptyExpressionObj.getContent()));
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class Unknown extends SourceLocation{
	
	private String type;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Unknown(String type) {
		this.type = type;
	}
	
}

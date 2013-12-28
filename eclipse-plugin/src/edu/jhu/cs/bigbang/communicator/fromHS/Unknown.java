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
	
	public String toString() {
		return "Unknown";
	}
 	
	public boolean equals(Unknown unknownObj) {
		if(this.type.equals(unknownObj.getType())) return true;
		else return false;
	}
	
}

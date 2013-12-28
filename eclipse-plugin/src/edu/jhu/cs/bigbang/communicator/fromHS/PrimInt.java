package edu.jhu.cs.bigbang.communicator.fromHS;

public class PrimInt extends PrimitiveType{
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public PrimInt(Origin origin) {
		super();
		this.origin = origin;
	}

	public String toString() {
		return  "int";
	}
	
	public boolean equals(PrimInt primIntObj) {
		if(this.origin.equals(primIntObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
}

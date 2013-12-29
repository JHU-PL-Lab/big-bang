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

	@Override
	public String toString() {
		return  "int";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof PrimInt)) return false;
		else return true;		
	}
}

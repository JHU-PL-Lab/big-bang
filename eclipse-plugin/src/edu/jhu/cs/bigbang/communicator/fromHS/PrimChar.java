package edu.jhu.cs.bigbang.communicator.fromHS;

public class PrimChar extends PrimitiveType{
	
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public PrimChar(Origin origin) {
		super();
		this.origin = origin;
	}
	
	@Override
	public String toString() {
		return  "char";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof PrimChar)) return false;
		else return true;
	}
	
}

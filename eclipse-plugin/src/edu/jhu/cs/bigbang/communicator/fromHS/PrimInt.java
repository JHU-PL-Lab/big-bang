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

}

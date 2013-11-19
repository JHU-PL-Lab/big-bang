package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class BinaryOperator {
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public BinaryOperator(Origin origin) {
		super();
		this.origin = origin;
	}
	
}

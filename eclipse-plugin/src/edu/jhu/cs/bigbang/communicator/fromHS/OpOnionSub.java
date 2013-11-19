package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpOnionSub extends OnionOp{
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public OpOnionSub(Origin origin) {
		super();
		this.origin = origin;
	}
}

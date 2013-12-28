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
	
	public String toString() {
		return "OpOnionSub";
	}
	
	public boolean equals(OpOnionSub opOnionSub) {
		if (this.origin.equals(opOnionSub.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
	
}

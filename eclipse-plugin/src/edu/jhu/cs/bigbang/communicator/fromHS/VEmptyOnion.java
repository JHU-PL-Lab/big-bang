package edu.jhu.cs.bigbang.communicator.fromHS;

public class VEmptyOnion extends Value{
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public VEmptyOnion(Origin origin) {
		super();
		this.origin = origin;
	}
	

}

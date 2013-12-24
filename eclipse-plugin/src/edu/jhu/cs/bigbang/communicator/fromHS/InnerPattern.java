package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class InnerPattern {
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public InnerPattern(Origin origin) {
		super();
		this.origin = origin;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class Projector {
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public Projector(Origin origin) {
		super();
		this.origin = origin;
	}
	
	public String toString() {
		return "Projector";
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class CellQualifier {
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public CellQualifier(Origin origin) {
		super();
		this.origin = origin;
	}
	
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class EvaluatedClause {
	
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public EvaluatedClause(Origin origin) {
		super();
		this.origin = origin;
	}
	
}

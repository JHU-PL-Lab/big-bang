package edu.jhu.cs.bigbang.communicator.fromHS;

public class InvalidExpressionEnd extends IllFormedness{
	
	private Clause clause;

	public Clause getClause() {
		return clause;
	}

	public void setClause(Clause clause) {
		this.clause = clause;
	}

	public InvalidExpressionEnd(Clause clause) {
		super();
		this.clause = clause;
	}
	
}

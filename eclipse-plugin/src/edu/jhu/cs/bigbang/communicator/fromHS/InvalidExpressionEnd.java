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
	
	@Override
	public String toString() {
		return " " + clause;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (!(obj instanceof InvalidExpressionEnd)) return false;
		InvalidExpressionEnd invalidExpressionEndObj = (InvalidExpressionEnd) obj;
		return (this.clause.equals(invalidExpressionEndObj.getClause()));
	}
	
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class Evaluated extends Clause{
	
	private EvaluatedClause evaluatedClause;

	public EvaluatedClause getEvaluatedClause() {
		return evaluatedClause;
	}

	public void setEvaluatedClause(EvaluatedClause evaluatedClause) {
		this.evaluatedClause = evaluatedClause;
	}

	public Evaluated(EvaluatedClause evaluatedClause) {
		super();
		this.evaluatedClause = evaluatedClause;
	}
	
	@Override
	public String toString() {
    	return " " + evaluatedClause;
    }
	
	public boolean equals(Object obj) { 
		if(obj == null) return false;
		if(! (obj instanceof Evaluated)) return false;
		Evaluated evaluatedObj = (Evaluated) obj;
		if(this.evaluatedClause.equals(evaluatedObj.getEvaluatedClause())) {				
			return true;
		} else {
			return false;
		}
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class IllFormedExpression extends EvalError{
	
	private IllFormedness illFormedness;

	public IllFormedness getIllFormedness() {
		return illFormedness;
	}

	public void setIllFormedness(IllFormedness illFormedness) {
		this.illFormedness = illFormedness;
	}

	public IllFormedExpression(IllFormedness illFormedness) {
		super();
		this.illFormedness = illFormedness;
	}
	
	@Override
	public String toString() {
		return " " + illFormedness;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (!(obj instanceof IllFormedExpression)) return false;
		IllFormedExpression illFormedExpressionObj = (IllFormedExpression) obj;
		if (this.illFormedness.equals(illFormedExpressionObj.getIllFormedness())) return true;
		else return false;
	}
}

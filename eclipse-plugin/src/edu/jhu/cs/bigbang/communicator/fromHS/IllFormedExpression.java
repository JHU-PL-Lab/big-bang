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
}

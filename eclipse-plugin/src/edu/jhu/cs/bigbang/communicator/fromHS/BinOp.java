package edu.jhu.cs.bigbang.communicator.fromHS;

public class BinOp extends Redex{
	
	private BinaryOperator binaryOperator;
	private AbstractFlowVar flowVar2;

	public BinaryOperator getBinaryOperator() {
		return binaryOperator;
	}
	
	public void setBinaryOperator(BinaryOperator binaryOperator) {
		this.binaryOperator = binaryOperator;
	}
	
	public AbstractFlowVar getFlowVar2() {
		return flowVar2;
	}
	
	public void setFlowVar2(AbstractFlowVar flowVar2) {
		this.flowVar2 = flowVar2;
	}
	
	public BinOp(Origin origin, AbstractFlowVar flowVar,
			BinaryOperator binaryOperator, AbstractFlowVar flowVar2) {
		super(origin, flowVar); 
		this.binaryOperator = binaryOperator;
		this.flowVar2 = flowVar2;
	}
	
	@Override
	public String toString() {
    	return " " + this.getFlowVar() + " " + binaryOperator + " " + flowVar2;
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		if (! (obj instanceof BinOp)) return false;
		BinOp binOpObj = (BinOp) obj;
		if(this.getFlowVar().equals(binOpObj.getFlowVar()) &&
		   this.getFlowVar2().equals(binOpObj.getFlowVar2()) &&
		   this.getBinaryOperator().equals(binOpObj.getBinaryOperator())) return true;
		else return false;   
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class ValueDef extends EvaluatedClause{
	
	private AbstractFlowVar flowVar;
	private Value value;
	
	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}
	
	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}
	
	public Value getValue() {
		return value;
	}
	
	public void setValue(Value value) {
		this.value = value;
	}
	
	public ValueDef(Origin origin, AbstractFlowVar flowVar, Value value) {
		super(origin);
		this.flowVar = flowVar;
		this.value = value;
	}
	
	public String toString() {
    	return " " + flowVar + " = " + value;
    }
	
	public boolean equals(ValueDef valueDefObj) {
		if(this.flowVar.equals(valueDefObj.getFlowVar()) &&
		   this.value.equals(valueDefObj.getValue()) &&
		   this.getOrigin().equals(valueDefObj.getOrigin())) return true;
		else return false;
	}
}

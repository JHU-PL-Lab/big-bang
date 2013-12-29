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
	
	@Override
	public String toString() {
    	return " " + flowVar + " = " + value;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof ValueDef)) return false;
		ValueDef valueDefObj = (ValueDef) obj;
		if(this.flowVar.equals(valueDefObj.getFlowVar()) &&
		   this.value.equals(valueDefObj.getValue())) return true;
		else return false;
	}
}

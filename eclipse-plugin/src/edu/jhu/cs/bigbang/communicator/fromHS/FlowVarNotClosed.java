package edu.jhu.cs.bigbang.communicator.fromHS;

public class FlowVarNotClosed extends EvalError{
	
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public FlowVarNotClosed(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}
	
	@Override
	public String toString() {
    	return " " + flowVar;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof FlowVarNotClosed)) return false;
		FlowVarNotClosed flowVarNotClosedObj = (FlowVarNotClosed) obj;
		if(this.flowVar.equals(flowVarNotClosedObj.getFlowVar())) return true;
		else return false;					
	}
}

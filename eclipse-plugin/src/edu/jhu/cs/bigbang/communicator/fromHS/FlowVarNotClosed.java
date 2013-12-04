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
	
}

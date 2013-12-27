package edu.jhu.cs.bigbang.communicator.fromHS;

public class SomeFlowVar extends AnyVar{
	
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public SomeFlowVar(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}

	public String toString() {
		return " " + flowVar;
	}
	
}

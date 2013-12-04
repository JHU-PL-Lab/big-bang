package edu.jhu.cs.bigbang.communicator.fromHS;

public class DuplicateFlowUse extends IllFormedness {
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public DuplicateFlowUse(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}
}

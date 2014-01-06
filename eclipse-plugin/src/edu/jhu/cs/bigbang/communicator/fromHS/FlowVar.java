package edu.jhu.cs.bigbang.communicator.fromHS;

public class FlowVar extends AbstractFlowVar{

	public FlowVar(Origin origin, String flowStr) {
		super(origin, flowStr);
	}
	
	@Override
	public String toString() {
		return this.getFlowStr();
	}
	
}

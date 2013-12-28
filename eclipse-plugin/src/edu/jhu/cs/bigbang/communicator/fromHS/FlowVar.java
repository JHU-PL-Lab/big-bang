package edu.jhu.cs.bigbang.communicator.fromHS;

public class FlowVar extends AbstractFlowVar{

	public FlowVar(Origin origin, String flowStr) {
		super(origin, flowStr);
	}
	public boolean equals(FlowVar flowVarObj) {
		if(this.getOrigin().equals(flowVarObj.getOrigin()) && this.getFlowStr().equals(flowVarObj.getFlowStr())) {
			return true;
		} else {
			return false;
		}
	}
}

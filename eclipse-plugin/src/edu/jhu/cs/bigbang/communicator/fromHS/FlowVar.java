package edu.jhu.cs.bigbang.communicator.fromHS;

public class FlowVar extends AbstractFlowVar{

	public FlowVar(Origin origin, String flowStr) {
		super(origin, flowStr);
	}
	
	@Override
	public String toString() {
		return this.getFlowStr();
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof FlowVar)) return false;
		FlowVar flowVarObj = (FlowVar) obj;
		if(this.getFlowStr().equals(flowVarObj.getFlowStr())) {
			return true;
		} else {
			return false;
		}
	}
}

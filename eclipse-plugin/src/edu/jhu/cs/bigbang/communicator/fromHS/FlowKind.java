package edu.jhu.cs.bigbang.communicator.fromHS;

public class FlowKind {
	private String flowKind;

	public String getFlowKind() {
		return flowKind;
	}

	public void setFlowKind(String flowKind) {
		this.flowKind = flowKind;
	}

	public FlowKind(String flowKind) {
		super();
		this.flowKind = flowKind;
	}
	
	public String toString() {
    	return flowKind;
    }
	
	public boolean equals(FlowKind flowKindObj) {
		if(this.flowKind.equals(flowKindObj.getFlowKind())) return true;
		else return false;
	}
}

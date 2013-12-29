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
	
	@Override
	public String toString() {
    	return flowKind;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof FlowKind)) return false;
		FlowKind flowKindObj = (FlowKind) obj;
		return (this.flowKind.equals(flowKindObj.getFlowKind()));
	}
}

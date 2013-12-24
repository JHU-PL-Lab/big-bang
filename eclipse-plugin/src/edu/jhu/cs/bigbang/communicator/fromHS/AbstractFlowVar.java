package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class AbstractFlowVar {
	private Origin origin;
	private String flowContents;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public String getFlowStr() {
		return flowContents;
	}
	
	public void setFlowStr(String flowStr) {
		this.flowContents = flowStr;
	}

	public AbstractFlowVar(Origin origin, String flowStr) {
		super();
		this.origin = origin;
		this.flowContents= flowStr;
	}
	
	public String toString() {
		return  flowContents;
	}
	
}

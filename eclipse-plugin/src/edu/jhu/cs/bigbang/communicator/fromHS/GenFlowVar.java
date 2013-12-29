package edu.jhu.cs.bigbang.communicator.fromHS;

public class GenFlowVar extends AbstractFlowVar{

	private int flowNum;

	public int getFlowVarInt() {
		return flowNum;
	}

	public void setFlowVarInt(int flowVarInt) {
		this.flowNum= flowVarInt;
	}

	public GenFlowVar(Origin origin, String flowStr, int flowVarInt) {
		super(origin, flowStr);
		this.flowNum = flowVarInt;
	}

	@Override
	public String toString() {
    	return " " + flowNum; 
    }

	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof GenFlowVar)) return false;
		GenFlowVar genFlowVarObj = (GenFlowVar) obj;
		if(this.getFlowStr().equals(genFlowVarObj.getFlowStr()) && 
		   this.getFlowVarInt() == genFlowVarObj.getFlowVarInt()) {
			return true;
		} else {
			return false;
		}
	}
	
}

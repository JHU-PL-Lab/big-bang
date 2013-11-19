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
	
}

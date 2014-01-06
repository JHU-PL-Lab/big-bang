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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + flowNum;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		GenFlowVar other = (GenFlowVar) obj;
		if (flowNum != other.flowNum)
			return false;
		return true;
	}

	
	
}

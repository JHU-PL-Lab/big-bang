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

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((flowContents == null) ? 0 : flowContents.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractFlowVar other = (AbstractFlowVar) obj;
		if (flowContents == null) {
			if (other.flowContents != null)
				return false;
		} else if (!flowContents.equals(other.flowContents))
			return false;
		return true;
	}
	
	
	
}

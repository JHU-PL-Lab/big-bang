package edu.jhu.cs.bigbang.communicator.fromHS;

public class Throws extends Clause{
	
	private Origin origin;
	private AbstractFlowVar flowVar;
	private AbstractFlowVar flowVar2;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}
	
	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}
	
	public AbstractFlowVar getFlowVar2() {
		return flowVar2;
	}
	
	public void setFlowVar2(AbstractFlowVar flowVar2) {
		this.flowVar2 = flowVar2;
	}
	
	public Throws(Origin origin, AbstractFlowVar flowVar,
			AbstractFlowVar flowVar2) {
		super();
		this.origin = origin;
		this.flowVar = flowVar;
		this.flowVar2 = flowVar2;
	}
	
	@Override
	public String toString() {
    	return " " + flowVar + " throws " + flowVar2;
    }
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof Throws)) return false;
		Throws throwsObj = (Throws) obj;
		if (this.flowVar.equals(throwsObj.getFlowVar()) &&
			this.flowVar2.equals(throwsObj.getFlowVar2())) return true;
		else return false;
	}
}

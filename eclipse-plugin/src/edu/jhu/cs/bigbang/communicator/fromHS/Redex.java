package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class Redex {
	
	private Origin origin;
	private AbstractFlowVar flowVar;
	
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
	
	public Redex(Origin origin, AbstractFlowVar flowVar) {
		super();
		this.origin = origin;
		this.flowVar = flowVar;
	}
	
}

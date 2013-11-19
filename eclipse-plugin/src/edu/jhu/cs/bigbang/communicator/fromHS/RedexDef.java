package edu.jhu.cs.bigbang.communicator.fromHS;

public class RedexDef extends Clause{
	private Origin origin;
	private AbstractFlowVar flowVar;
	private Redex redex;
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
	public Redex getRedex() {
		return redex;
	}
	public void setRedex(Redex redex) {
		this.redex = redex;
	}
	public RedexDef(Origin origin, AbstractFlowVar flowVar, Redex redex) {
		super();
		this.origin = origin;
		this.flowVar = flowVar;
		this.redex = redex;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class VOnionFilter extends Value {

	private Origin origin;
	private AbstractFlowVar flowVar;
	private OnionOp onionOp;
	private AnyProjector anyProjector;

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

	public OnionOp getOnionOp() {
		return onionOp;
	}

	public void setOnionOp(OnionOp onionOp) {
		this.onionOp = onionOp;
	}

	public AnyProjector getAnyProjector() {
		return anyProjector;
	}

	public void setAnyProjector(AnyProjector anyProjector) {
		this.anyProjector = anyProjector;
	}

	public VOnionFilter(Origin origin, AbstractFlowVar flowVar, OnionOp onionOp,
			AnyProjector anyProjector) {
		super();
		this.origin = origin;
		this.flowVar = flowVar;
		this.onionOp = onionOp;
		this.anyProjector = anyProjector;
	}
	
	public String toString() {
		return flowVar + "" + onionOp + " " + anyProjector + " ";
	}
	
	public boolean equals(VOnionFilter vOnionFilter) {
		if(this.origin.equals(vOnionFilter.getOrigin()) && 
		   this.flowVar.equals(vOnionFilter.getFlowVar()) && 
		   this.onionOp.equals(vOnionFilter.getOnionOp()) && 
		   this.anyProjector.equals(vOnionFilter.getAnyProjector())) {
			
			return true;
		} else {
			return false;
		}
	}
	
}
	
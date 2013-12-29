package edu.jhu.cs.bigbang.communicator.fromHS;

public class Appl extends Redex{
	
	private AbstractFlowVar flowVar2;

	public AbstractFlowVar getFlowVar2() {
		return flowVar2;
	}
	
	public void setFlowVar2(AbstractFlowVar flowVar2) {
		this.flowVar2 = flowVar2;
	}
	
	public Appl(Origin origin, AbstractFlowVar flowVar1,
			AbstractFlowVar flowVar2) {
		super(origin, flowVar1);		
		this.flowVar2 = flowVar2;
	}
	
	@Override
	public String toString() {
    	return " " + this.getFlowVar() + " " + flowVar2;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof Appl)) return false;
		Appl applObj = (Appl) obj;
		if(this.getFlowVar().equals(applObj.getFlowVar()) && 
		   this.getFlowVar2().equals(applObj.getFlowVar2())) return true;
		else return false;
	}
}

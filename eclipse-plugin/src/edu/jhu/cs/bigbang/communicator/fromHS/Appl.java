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
	
	public String toString() {
    	return " " + this.getFlowVar() + " " + flowVar2;
    }
	
	public boolean equals(Appl applObject) { 
		if(this.getOrigin().equals(applObject.getOrigin()) && 
		   this.getFlowVar().equals(applObject.getFlowVar()) && 
		   this.getFlowVar2().equals(applObject.getFlowVar2())) return true;
		else return false;
	}
}

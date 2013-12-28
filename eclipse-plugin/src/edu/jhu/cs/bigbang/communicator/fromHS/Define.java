package edu.jhu.cs.bigbang.communicator.fromHS;

public class Define extends Redex {
	
	public Define(Origin origin, AbstractFlowVar flowVar) {
		super(origin, flowVar);
	}
	
	public String toString() {
    	return " " + this.getFlowVar();
    }
	
	public boolean equals(Define defineObj) {
		if(this.getOrigin().equals(defineObj.getOrigin()) &&
		   this.getFlowVar().equals(defineObj.getFlowVar())) return true;
		else return false;
	}
}

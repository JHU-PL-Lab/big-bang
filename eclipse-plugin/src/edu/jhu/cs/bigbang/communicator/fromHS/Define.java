package edu.jhu.cs.bigbang.communicator.fromHS;

public class Define extends Redex {
	
	public Define(Origin origin, AbstractFlowVar flowVar) {
		super(origin, flowVar);
	}
	
	@Override
	public String toString() {
    	return " " + this.getFlowVar();
    }
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof Define)) return false;		
		Define defineObj = (Define) obj;
		if(this.getFlowVar().equals(defineObj.getFlowVar())) return true;
		else return false;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProjFun extends Projector{

	public ProjFun(Origin origin) {
		super(origin);
	}
	
	public String toString() {
		return "fun";
	}
	
	public boolean equals(ProjFun projFunObj) {
		if(this.getOrigin().equals(projFunObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
	
}

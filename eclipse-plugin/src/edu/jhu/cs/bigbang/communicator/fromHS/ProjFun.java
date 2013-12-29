package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProjFun extends Projector{

	public ProjFun(Origin origin) {
		super(origin);
	}
	
	@Override
	public String toString() {
		return "fun";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof ProjFun)) return false;
		else return true;		
	}
	
}

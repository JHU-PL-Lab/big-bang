package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpEqual extends BinaryOperator{

	public OpEqual(Origin origin) {
		super(origin);
	}
	
	@Override
	public String toString() {
		return "==";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpEqual)) return false;
		else return true;
	}
}

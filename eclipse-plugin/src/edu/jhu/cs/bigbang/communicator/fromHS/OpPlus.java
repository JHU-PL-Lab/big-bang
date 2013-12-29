package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpPlus extends BinaryOperator{

	public OpPlus(Origin origin) {
		super(origin);
	}

	@Override
	public String toString() {
		return "+";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpPlus)) return false;
		else return true;
	}
	
}

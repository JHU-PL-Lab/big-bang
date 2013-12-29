package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpMult extends BinaryOperator{

	public OpMult(Origin origin) {
		super(origin);
	}

	@Override
	public String toString() {
		return "*";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpMult)) return false;
		else return true;
	}
	
}

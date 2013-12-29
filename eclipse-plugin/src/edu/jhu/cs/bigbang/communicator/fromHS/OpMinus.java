package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpMinus extends BinaryOperator{

	public OpMinus(Origin origin) {
		super(origin);
	}

	@Override
	public String toString() {
		return "-";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpMinus)) return false;
		else return true;
	}
	
}

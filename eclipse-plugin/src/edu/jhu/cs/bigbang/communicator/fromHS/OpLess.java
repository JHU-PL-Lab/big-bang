package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpLess extends BinaryOperator{

	public OpLess(Origin origin) {
		super(origin);
	}

	@Override
	public String toString() {
		return "<";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpLess)) return false;
		else return true;
	}
	
}

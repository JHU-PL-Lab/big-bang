package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpGreater extends BinaryOperator{

	public OpGreater(Origin origin) {
		super(origin);
	}

	@Override
	public String toString() {
		return ">";
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(! (obj instanceof OpGreater)) return false;
		else return true;
	}
	
}

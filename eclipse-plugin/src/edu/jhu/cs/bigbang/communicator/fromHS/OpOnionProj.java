package edu.jhu.cs.bigbang.communicator.fromHS;

public class OpOnionProj extends OnionOp{
	
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public OpOnionProj(Origin origin) {
		super();
		this.origin = origin;
	}
    
	@Override
	public String toString() {
		return "OpOnionPoj";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof OpOnionProj)) return false;
		else return true;
	}
	
}

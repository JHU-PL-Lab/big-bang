package edu.jhu.cs.bigbang.communicator.fromHS;

public class EmptyOnionPattern extends InnerPattern{

	public EmptyOnionPattern(Origin origin) {
		super(origin);
	}
    
	@Override
	public String toString() {		
		return "()";		
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof EmptyOnionPattern)) return false;
		else return true;
	}
	
}

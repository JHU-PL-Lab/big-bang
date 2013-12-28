package edu.jhu.cs.bigbang.communicator.fromHS;

public class EmptyOnionPattern extends InnerPattern{

	public EmptyOnionPattern(Origin origin) {
		super(origin);
	}
    
	public String toString() {		
		return "()";		
	}
	
	public boolean equals(EmptyOnionPattern emptyOnionPatternObj) {
		if (this.getOrigin().equals(emptyOnionPatternObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
	
}

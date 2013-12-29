package edu.jhu.cs.bigbang.communicator.fromHS;

public class ScapePattern extends InnerPattern{

	public ScapePattern(Origin origin) {
		super(origin);
	} 
	
	@Override
    public String toString() {
    	return "fun";
    }
    
    public boolean equals(Object obj) {
    	if (obj == null) return false;
    	else if (! (obj instanceof ScapePattern)) return false;
    	else return true;
    }
}

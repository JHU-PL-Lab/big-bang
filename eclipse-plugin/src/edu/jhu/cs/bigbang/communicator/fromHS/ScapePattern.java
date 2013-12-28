package edu.jhu.cs.bigbang.communicator.fromHS;

public class ScapePattern extends InnerPattern{

	public ScapePattern(Origin origin) {
		super(origin);
	} 
    public String toString() {
    	return "fun";
    }
    
    public boolean equals(ScapePattern scapePatternObj) {
    	if(this.getOrigin().equals(scapePatternObj.getOrigin())) {
    		return true;
    	}else {
    		return false;
    	}
    }
}

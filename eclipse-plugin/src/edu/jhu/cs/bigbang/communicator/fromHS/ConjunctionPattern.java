package edu.jhu.cs.bigbang.communicator.fromHS;

public class ConjunctionPattern extends InnerPattern{
	
	private InnerPattern innerPattern;
	private InnerPattern innerPattern2;
	
	public InnerPattern getInnerPattern() {
		return innerPattern;
	}
	public void setInnerPattern(InnerPattern innerPattern) {
		this.innerPattern = innerPattern;
	}
	public InnerPattern getInnerPattern2() {
		return innerPattern2;
	}
	public void setInnerPattern2(InnerPattern innerPattern2) {
		this.innerPattern2 = innerPattern2;
	}
	public ConjunctionPattern(Origin origin, InnerPattern innerPattern,
			InnerPattern innerPattern2) {
		super(origin);
		this.innerPattern = innerPattern;
		this.innerPattern2 = innerPattern2;
	}
    
	public String toString() {
		return "(" + innerPattern + ") & (" + innerPattern2 + ")";  
	}
	
	public boolean equals(ConjunctionPattern conjunctionPatternObj) {
		if(this.innerPattern.equals(conjunctionPatternObj.getInnerPattern()) &&
		   this.innerPattern2.equals(conjunctionPatternObj.getInnerPattern2())) {
			return true;
		}else {
			return false;
		}
	}
}

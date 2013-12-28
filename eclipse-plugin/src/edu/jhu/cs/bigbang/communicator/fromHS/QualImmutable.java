package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualImmutable extends CellQualifier{

	public QualImmutable(Origin origin) {
		super(origin);
	}
	
	public String toString() {
    	return "QualImmutable";
    }
	
	public boolean equals(QualImmutable qualImmutableObj) { 
		if(this.getOrigin().equals(qualImmutableObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
}

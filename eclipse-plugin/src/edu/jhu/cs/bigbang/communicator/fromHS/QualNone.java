package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualNone extends CellQualifier{

	public QualNone(Origin origin) {
		super(origin);
	}
	
	public String toString() {
    	return "QualNone";
    }
	
	public boolean equals(QualNone qualNoneObj) { 
		if(this.getOrigin().equals(qualNoneObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
	
}

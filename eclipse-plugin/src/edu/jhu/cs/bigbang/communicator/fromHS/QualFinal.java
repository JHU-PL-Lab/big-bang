package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualFinal extends CellQualifier{

	public QualFinal(Origin origin) {
		super(origin);
	}
	
	public String toString() {
    	return "QualFinal";
    }
	
	public boolean equals(QualFinal qualFinalObj) { 
		if(this.getOrigin().equals(qualFinalObj.getOrigin())) {
			return true;
		} else {
			return false;
		}
	}
}

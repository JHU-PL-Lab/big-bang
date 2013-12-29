package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualNone extends CellQualifier{

	public QualNone(Origin origin) {
		super(origin);
	}
	
	@Override
	public String toString() {
    	return "QualNone";
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		else if (! (obj instanceof QualNone)) return false;
		else return true;
	}
	
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualImmutable extends CellQualifier{

	public QualImmutable(Origin origin) {
		super(origin);
	}
	
	@Override
	public String toString() {
    	return "QualImmutable";
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		else if (! (obj instanceof QualImmutable)) return false;
		else return true;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class QualFinal extends CellQualifier{

	public QualFinal(Origin origin) {
		super(origin);
	}
	
	@Override
	public String toString() {
    	return "QualFinal";
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		else if(!(obj instanceof QualFinal)) return false;
		else return true;
	}
}

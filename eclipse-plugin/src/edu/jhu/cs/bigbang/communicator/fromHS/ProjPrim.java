package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProjPrim extends Projector{
	private PrimitiveType primitiveType;

	public PrimitiveType getPrimitiveType() {
		return primitiveType;
	}

	public void setPrimitiveType(PrimitiveType primitiveType) {
		this.primitiveType = primitiveType;
	}

	public ProjPrim(Origin origin, PrimitiveType primitiveType) {
		super(origin);
		this.primitiveType = primitiveType;
	}
	
	public String toString() {
		return " " + primitiveType;
	}
	
	public boolean equals(ProjPrim projPrimObj) {
		if(this.getOrigin().equals(projPrimObj.getOrigin()) && this.primitiveType.equals(projPrimObj.getPrimitiveType())) {			
			return true;
		} else {
			return false;
		}
	}
	
}

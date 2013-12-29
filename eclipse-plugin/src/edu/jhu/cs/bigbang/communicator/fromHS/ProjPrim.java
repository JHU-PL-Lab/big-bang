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
	
	@Override
	public String toString() {
		return " " + primitiveType;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof ProjPrim)) return false;
		ProjPrim projPrimObj = (ProjPrim) obj;
		if(this.primitiveType.equals(projPrimObj.getPrimitiveType())) {			
			return true;
		} else {
			return false;
		}
	}
	
}

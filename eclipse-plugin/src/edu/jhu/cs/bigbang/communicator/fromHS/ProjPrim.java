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
}

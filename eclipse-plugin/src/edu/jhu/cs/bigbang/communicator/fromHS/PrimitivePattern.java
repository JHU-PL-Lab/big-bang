package edu.jhu.cs.bigbang.communicator.fromHS;

public class PrimitivePattern extends InnerPattern{
	
	private PrimitiveType primitiveType;
	
	
	public PrimitiveType getPrimitiveType() {
		return primitiveType;
	}

	public void setPrimitiveType(PrimitiveType primitiveType) {
		this.primitiveType = primitiveType;
	}

	public PrimitivePattern(Origin origin, PrimitiveType primitiveType) {
		super(origin);
		this.primitiveType = primitiveType;
	}

	public String toString() {
		return  primitiveType + "";
	}
	
}

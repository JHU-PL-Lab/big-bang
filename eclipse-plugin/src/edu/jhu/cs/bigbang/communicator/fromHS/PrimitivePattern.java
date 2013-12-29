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
	
	@Override
	public String toString() {
		return  primitiveType + "";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof PrimitivePattern)) return false;
		PrimitivePattern primitivePatternObj = (PrimitivePattern) obj;
		if(this.primitiveType.equals(primitivePatternObj.getPrimitiveType())) {
			return true;
		}else {
			return false;
		}
	}
	
}

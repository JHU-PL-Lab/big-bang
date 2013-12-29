package edu.jhu.cs.bigbang.communicator.fromHS;

public class VInt extends Value{
	
	private Origin origin;
	private int intVar;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public int getIntVar() {
		return intVar;
	}
	
	public void setIntVar(int intVar) {
		this.intVar = intVar;
	}
	
	public VInt(Origin origin, int intVar) {
		super();
		this.origin = origin;
		this.intVar = intVar;
	}

	@Override
	public String toString() {
		return intVar + "";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof VInt)) return false;
        VInt vIntObj = (VInt) obj;		
		if(this.intVar == vIntObj.getIntVar() && this.origin.equals(vIntObj.getOrigin())) {
			return true;
		}else {
			return false;
		}
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.ArrayList;

public class OpenExpression extends EvalError{
	
	private ArrayList<AnyVar> anyVarLst;

	public ArrayList<AnyVar> getAnyVarLst() {
		return anyVarLst;
	}

	public void setAnyVarLst(ArrayList<AnyVar> anyVarLst) {
		this.anyVarLst = anyVarLst;
	}

	public OpenExpression(ArrayList<AnyVar> anyVarLst) {
		super();
		this.anyVarLst = anyVarLst;
	}
}

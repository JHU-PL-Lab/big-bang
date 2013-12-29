package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.ArrayList;
import java.util.Iterator;

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
	
	@Override
	public String toString() {
		Iterator arrLstI = anyVarLst.iterator();
		StringBuffer resultStr = null;
		while(arrLstI.hasNext()) {
			resultStr.append(" " + arrLstI.next());
		}
		return resultStr.toString();
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof OpenExpression)) return false;
		OpenExpression openExpressionObj = (OpenExpression) obj;
		return (this.anyVarLst.equals(openExpressionObj.getAnyVarLst()));
	}
}

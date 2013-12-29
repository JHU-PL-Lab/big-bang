package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.ArrayList;
import java.util.Iterator;

public class Expr {
	
	private Origin origin;
	private ArrayList<Clause> clauseLst;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public ArrayList<Clause> getClauseLst() {
		return clauseLst;
	}
	
	public void setClauseLst(ArrayList<Clause> clauseLst) {
		this.clauseLst = clauseLst;
	}
	
	public Expr(Origin origin, ArrayList<Clause> clauseLst) {
		super();
		this.origin = origin;
		this.clauseLst = clauseLst;
	}
	
	@Override
	public String toString() {
		StringBuffer resultStr = null;
		Clause tmpClause= null;
		resultStr.append("");
		Iterator<Clause> clauseI = clauseLst.iterator();
		while (clauseI.hasNext()) {
			if (!tmpClause.equals(null)) 
				resultStr.append(tmpClause + "; ");
			tmpClause = clauseI.next();	
		}
		resultStr.append(tmpClause);
		return resultStr.toString();
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof Expr)) return false;
		Expr exprObj = (Expr) obj;
		if(this.clauseLst.equals(exprObj.getClauseLst())) {
			return true;
		} else {
			return false;
		}
	}
}

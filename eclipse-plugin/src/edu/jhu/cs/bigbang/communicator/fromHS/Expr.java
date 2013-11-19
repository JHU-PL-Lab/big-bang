package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.ArrayList;

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
	
}

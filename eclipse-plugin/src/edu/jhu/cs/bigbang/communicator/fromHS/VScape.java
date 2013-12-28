package edu.jhu.cs.bigbang.communicator.fromHS;


public class VScape extends Value{
	
	private Origin origin;
	private Pattern pattern;
	private Expr expr;
	
	public Origin getOrigin() {
		return origin;
	}
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	public Pattern getPattern() {
		return pattern;
	}
	public void setPattern(Pattern pattern) {
		this.pattern = pattern;
	}
	public Expr getExpr() {
		return expr;
	}
	public void setExpr(Expr expr) {
		this.expr = expr;
	}
	public VScape(Origin origin, Pattern pattern, Expr expr) {
		super();
		this.origin = origin;
		this.pattern = pattern;
		this.expr = expr;
	}
	public String toString() {
		return  pattern + " " + expr;
	}
	
	public boolean equals(VScape vScapeObj) {
		if (this.origin.equals(vScapeObj.getOrigin()) &&
			this.pattern.equals(vScapeObj.getPattern())	&&
			this.expr.equals(vScapeObj.getExpr())) {
			return true;
		}else {
			return false;
		}
	}
}

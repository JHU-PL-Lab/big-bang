package edu.jhu.cs.bigbang.communicator.fromHS;

public class VChar extends Value{
	private Origin origin;
	private char charVar;
	public Origin getOrigin() {
		return origin;
	}
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	public char getCharVar() {
		return charVar;
	}
	public void setCharVar(char charVar) {
		this.charVar = charVar;
	}
	public VChar(Origin origin, char charVar) {
		super();
		this.origin = origin;
		this.charVar = charVar;
	}	

}

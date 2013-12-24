package edu.jhu.cs.bigbang.communicator.fromHS;

public class LabelName {
	private Origin origin;
	private String nameStr;
	public Origin getOrigin() {
		return origin;
	}
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	public String getNameStr() {
		return nameStr;
	}
	public void setNameStr(String nameStr) {
		this.nameStr = nameStr;
	}
	public LabelName(Origin origin, String nameStr) {
		super();
		this.origin = origin;
		this.nameStr = nameStr;
	}
	
	public String toString() {
		return nameStr + "";
	}
	
}

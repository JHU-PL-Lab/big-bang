package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class AbstractCellVar {

	private Origin origin;
	private String cellContents;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public String getCellVarStr() {
		return cellContents;
	}
	
	public void setCellVarStr(String cellVarStr) {
		this.cellContents= cellVarStr;
	}
	
	public AbstractCellVar(Origin origin, String cellVarStr) {
		super();
		this.origin = origin;
		this.cellContents= cellVarStr;
	}
	
	
	public String toString() {
		return cellContents;
	}
 }

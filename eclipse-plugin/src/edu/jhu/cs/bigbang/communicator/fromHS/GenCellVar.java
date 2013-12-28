package edu.jhu.cs.bigbang.communicator.fromHS;

public class GenCellVar extends AbstractCellVar{
	
	private int cellNum;

	public int getCellInt() {
		return cellNum;
	}
	
	public void setCellInt(int cellInt) {
		this.cellNum= cellInt;
	}
	
	public GenCellVar(Origin origin, String cellStr, int cellInt) {
		super(origin, cellStr);
		this.cellNum = cellInt;
	}
	
	public String toString() {
    	return " " + cellNum;
    }
	
	public boolean equals(GenCellVar genCellVarObj) {
		if(this.getOrigin().equals(genCellVarObj.getOrigin()) && this.getCellVarStr().equals(genCellVarObj.getCellVarStr()) && this.getCellInt() == genCellVarObj.getCellInt()) {
			return true;
		} else {
			return false;
		}
	}
	
}

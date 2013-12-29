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
	
	@Override
	public String toString() {
    	return " " + cellNum;
    }
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof GenCellVar)) return false;
		GenCellVar genCellVarObj = (GenCellVar) obj; 
		if(this.getCellVarStr().equals(genCellVarObj.getCellVarStr()) && 
		   this.getCellInt() == genCellVarObj.getCellInt()) {
			return true;
		} else {
			return false;
		}
	}
	
}

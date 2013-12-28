package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellVar extends AbstractCellVar{

	public CellVar(Origin origin, String cellVarStr) {
		super(origin, cellVarStr);
	}
	
	public boolean equals(CellVar cellVarObj) {
		if(this.getOrigin().equals(cellVarObj.getOrigin()) && this.getCellVarStr().equals(cellVarObj.getCellVarStr())) {
			return true;
		} else {
			return false;
		}
	}
}

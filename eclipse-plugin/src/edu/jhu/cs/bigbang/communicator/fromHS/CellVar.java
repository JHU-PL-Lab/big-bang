package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellVar extends AbstractCellVar{

	public CellVar(Origin origin, String cellVarStr) {
		super(origin, cellVarStr);
	}
	
	@Override
	public String toString() {
		return this.getCellVarStr();
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof CellVar)) return false;
		CellVar cellVarObj = (CellVar) obj;
		if(this.getCellVarStr().equals(cellVarObj.getCellVarStr())) {
			return true;
		} else {
			return false;
		}
	}
}

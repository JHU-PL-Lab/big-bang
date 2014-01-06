package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellVar extends AbstractCellVar{

	public CellVar(Origin origin, String cellVarStr) {
		super(origin, cellVarStr);
	}
	
	@Override
	public String toString() {
		return this.getCellVarStr();
	}
	
}

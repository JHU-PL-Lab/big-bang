package edu.jhu.cs.bigbang.communicator.fromHS;

public class SomeCellVar extends AnyVar{
	
	private AbstractCellVar cellVar;

	public AbstractCellVar getCellVar() {
		return cellVar;
	}

	public void setCellVar(AbstractCellVar cellVar) {
		this.cellVar = cellVar;
	}

	public SomeCellVar(AbstractCellVar cellVar) {
		super();
		this.cellVar = cellVar;
	}
	
	public String toString() {
		return " " + cellVar;
	}
	
	public boolean equals(SomeCellVar someCellVarObj) {
		if(this.cellVar.equals(someCellVarObj.getCellVar())) {
			return true;
		}else {
			return false;
		}
	}
	
}
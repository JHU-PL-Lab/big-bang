package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellVarNotClosed extends EvalError{ 
	
	private AbstractCellVar cellVar;

	public AbstractCellVar getCellVar() {
		return cellVar;
	}

	public void setCellVar(AbstractCellVar cellVar) {
		this.cellVar = cellVar;
	}

	public CellVarNotClosed(AbstractCellVar cellVar) {
		super();
		this.cellVar = cellVar;
	}

}

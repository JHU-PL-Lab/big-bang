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
	
	@Override
	public String toString() {
    	return " " + cellVar;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof CellVarNotClosed)) return false;
		CellVarNotClosed cellVarNotClosedObj= (CellVarNotClosed) obj;
		if(this.cellVar.equals(cellVarNotClosedObj.getCellVar())) return true;
		else return false;					
	}
}

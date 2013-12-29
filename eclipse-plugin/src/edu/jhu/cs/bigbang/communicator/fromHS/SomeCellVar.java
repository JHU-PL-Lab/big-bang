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
	
	@Override
	public String toString() {
		return " " + cellVar;
	}
	
	public boolean equals(Object obj) {
		if (obj == null ) return false;
		if (! (obj instanceof SomeCellVar)) return false;
		SomeCellVar someCellVarObj = (SomeCellVar) obj;
		if(this.cellVar.equals(someCellVarObj.getCellVar())) {
			return true;
		}else {
			return false;
		}
	}
	
}
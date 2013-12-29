package edu.jhu.cs.bigbang.communicator.fromHS;

public class DuplicateCellBinding extends IllFormedness{
	
	private AbstractCellVar cellVar;

	public AbstractCellVar getCellVar() {
		return cellVar;
	}

	public void setCellVar(AbstractCellVar cellVar) {
		this.cellVar = cellVar;
	}

	public DuplicateCellBinding(AbstractCellVar cellVar) {
		super();
		this.cellVar = cellVar;
	}
	
	@Override
	public String toString() {
    	return " " + cellVar;
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		if (! (obj instanceof DuplicateCellBinding)) return false;
		DuplicateCellBinding duplicateCellBindingObj = (DuplicateCellBinding) obj;
		return (this.cellVar.equals(duplicateCellBindingObj.getCellVar()));		
	}
}

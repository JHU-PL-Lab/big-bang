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
	

}

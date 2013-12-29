package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellGet extends Clause{
	
	private Origin origin;
	private AbstractFlowVar flowVar;
	private AbstractCellVar cellVar;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}
	
	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}
	
	public AbstractCellVar getCellVar() {
		return cellVar;
	}
	
	public void setCellVar(AbstractCellVar cellVar) {
		this.cellVar = cellVar;
	}
	
	public CellGet(Origin origin, AbstractFlowVar flowVar,
			AbstractCellVar cellVar) {
		super();
		this.origin = origin;
		this.flowVar = flowVar;
		this.cellVar = cellVar;
	}
	
	@Override
	public String toString() {
    	return " " + cellVar + " =! " + flowVar;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
		if(! (obj instanceof CellGet)) return false;
		CellGet cellGetObj = (CellGet) obj;
		if(this.flowVar.equals(cellGetObj.getFlowVar()) &&
		   this.cellVar.equals(cellGetObj.getCellVar())) {
			return true;
		} else {
			return false;
		}
	}
}

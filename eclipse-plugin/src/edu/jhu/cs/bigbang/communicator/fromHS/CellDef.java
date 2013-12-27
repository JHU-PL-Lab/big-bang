package edu.jhu.cs.bigbang.communicator.fromHS;

public class CellDef extends EvaluatedClause{
	
	private CellQualifier cellQualifier;
	private AbstractFlowVar flowVar;
	private AbstractCellVar cellVar;
	
	public CellQualifier getCellQualifier() {
		return cellQualifier;
	}
	
	public void setCellQualifier(CellQualifier cellQualifier) {
		this.cellQualifier = cellQualifier;
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
	
	public CellDef(Origin origin, CellQualifier cellQualifier,
			AbstractFlowVar flowVar, AbstractCellVar cellVar) {
		super(origin);
		this.cellQualifier = cellQualifier;
		this.flowVar = flowVar;
		this.cellVar = cellVar;
	}
	
	public String toString() {
    	return " " + cellQualifier + " " + flowVar + " := " + cellVar;
    }
	
}

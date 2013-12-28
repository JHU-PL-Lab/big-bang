package edu.jhu.cs.bigbang.communicator.fromHS;

public class ExnPattern extends Pattern{
	
	private Origin origin;
	private AbstractCellVar cellVar;
	private InnerPattern innerPattern;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public AbstractCellVar getCellVar() {
		return cellVar;
	}
	
	public void setCellVar(AbstractCellVar cellVar) {
		this.cellVar = cellVar;
	}
	
	public InnerPattern getInnerPattern() {
		return innerPattern;
	}
	
	public void setInnerPattern(InnerPattern innerPattern) {
		this.innerPattern = innerPattern;
	}
	
	public ExnPattern(Origin origin, AbstractCellVar cellVar,
			InnerPattern innerPattern) {
		super();
		this.origin = origin;
		this.cellVar = cellVar;
		this.innerPattern = innerPattern;
	}
	
	public String toString() {
		return  cellVar + " " + innerPattern;
	}
	
	public boolean equals(ExnPattern exnPatternObj) {
		if(this.origin.equals(exnPatternObj.getOrigin()) &&
		   this.cellVar.equals(exnPatternObj.getCellVar()) &&
		   this.innerPattern.equals(exnPatternObj.getInnerPattern())) {
			return true;
		}else {
			return false;
		}
	}
}

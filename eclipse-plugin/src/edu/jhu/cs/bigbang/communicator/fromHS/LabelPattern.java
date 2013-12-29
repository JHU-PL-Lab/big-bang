package edu.jhu.cs.bigbang.communicator.fromHS;

public class LabelPattern extends InnerPattern{
	
	private LabelName labelName;
	private AbstractCellVar cellVar;
	private InnerPattern innerPattern;
	
	public LabelName getLabelName() {
		return labelName;
	}
	public void setLabelName(LabelName labelName) {
		this.labelName = labelName;
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
	public LabelPattern(Origin origin, LabelName labelName,
			AbstractCellVar cellVar, InnerPattern innerPattern) {
		super(origin);
		this.labelName = labelName;
		this.cellVar = cellVar;
		this.innerPattern = innerPattern;
	}	

	@Override
	public String toString() {
		return labelName + " " + cellVar + ":" + innerPattern;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof LabelPattern)) return false;
		LabelPattern labelPatternObj = (LabelPattern) obj;
		if(this.labelName.equals(labelPatternObj.getLabelName()) &&
		   this.cellVar.equals(labelPatternObj.getCellVar()) &&
		   this.innerPattern.equals(labelPatternObj.getInnerPattern())) {
			return true;
		}else {
			return false;
		}
	}
	
}

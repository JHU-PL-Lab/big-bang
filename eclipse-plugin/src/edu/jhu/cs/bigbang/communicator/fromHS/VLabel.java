package edu.jhu.cs.bigbang.communicator.fromHS;

public class VLabel extends Value{
	private Origin origin;
	private LabelName labelName;
	private AbstractCellVar cellVar;
	
	public Origin getOrigin() {
		return origin;
	}
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
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
	public VLabel(Origin origin, LabelName labelName, AbstractCellVar abstractCellVar) {
		super();
		this.origin = origin;
		this.labelName = labelName;
		this.cellVar = abstractCellVar;
	}
	
	public String toString() {
		return  "`" + labelName + " " + cellVar;
	}
}

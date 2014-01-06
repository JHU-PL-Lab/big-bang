package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.eclipse.toploop.ValueEnvironment;

public class VLabel extends Value {

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

	public VLabel(Origin origin, LabelName labelName,
			AbstractCellVar abstractCellVar) {
		super();
		this.origin = origin;
		this.labelName = labelName;
		this.cellVar = abstractCellVar;
	}

	@Override
	public String toString() {
		return "`" + labelName + " " + cellVar;
	}

	public String toDisplayString(ValueEnvironment env) {
		return "`" + labelName + " (" + env.displayStringOf(cellVar) + ")";
	}

	public boolean equals(Object obj) {
		if (obj == null)
			return false;
		if (!(obj instanceof VLabel))
			return false;
		VLabel vLabelObj = (VLabel) obj;
		if (this.labelName.equals(vLabelObj.labelName)
				&& this.cellVar.equals(vLabelObj.getCellVar())) {
			return true;
		} else {
			return false;
		}
	}

}

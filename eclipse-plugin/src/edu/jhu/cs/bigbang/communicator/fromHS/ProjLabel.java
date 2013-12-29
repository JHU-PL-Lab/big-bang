package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProjLabel extends Projector{
	
	private LabelName labelName;

	public LabelName getLabelName() {
		return labelName;
	}

	public void setLabelName(LabelName labelName) {
		this.labelName = labelName;
	}

	public ProjLabel(Origin origin, LabelName labelName) {
		super(origin);
		this.labelName = labelName;
	}
	
	@Override
	public String toString() {
		return " " + labelName;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof ProjLabel)) return false;
		ProjLabel projLabelObj = (ProjLabel) obj;
		if(this.labelName.equals(projLabelObj.getLabelName())) {
			return true;
		} else {
			return false;
		}
	}
}

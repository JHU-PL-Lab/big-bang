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
}

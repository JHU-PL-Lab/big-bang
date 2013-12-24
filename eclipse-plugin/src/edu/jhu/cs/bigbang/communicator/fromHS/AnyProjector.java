package edu.jhu.cs.bigbang.communicator.fromHS;

public class AnyProjector {
	
	private Projector projector;

	public Projector getProjector() {
		return projector;
	}

	public void setProjector(Projector projector) {
		this.projector = projector;
	}

	public AnyProjector(Projector projector) {
		super();
		this.projector = projector;
	}
	
	public String toString() {
		return projector + " ";
	}
	
}

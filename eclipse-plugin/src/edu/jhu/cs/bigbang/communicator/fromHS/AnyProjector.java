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
	
	@Override
	public String toString() {
		return projector + " ";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof AnyProjector)) return false;
		AnyProjector anyProjObj = (AnyProjector) obj;
		if(this.projector.equals(anyProjObj.getProjector())){
			return true;
		} else {
			return false;
		}
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class SourceOrigin extends Origin {
	
	private SourceRegion srcRegion;

	public SourceRegion getSrcRegion() {
		return srcRegion;
	}

	public void setSrcRegion(SourceRegion srcRegion) {
		this.srcRegion = srcRegion;
	}

	public SourceOrigin(SourceRegion srcRegion) {
		super();
		this.srcRegion = srcRegion;
	}
	
	public String toString() {
		return " " + srcRegion;
	}
	
	public boolean equals(SourceOrigin srcOriginObj) {
		if(this.srcRegion.equals(srcOriginObj.getSrcRegion())) return true;
		else return false;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class SourceRegion {
	
	private SourceLocation startPosition;
	private SourceLocation endPosition;
	
	public SourceRegion(SourceLocation startLoc, SourceLocation endLoc) {
		super();
		this.startPosition = startLoc;
		this.endPosition = endLoc;
	}
	
}

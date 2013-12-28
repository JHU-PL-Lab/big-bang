package edu.jhu.cs.bigbang.communicator.fromHS;

public class SourceRegion {
	
	private SourceLocation startPosition;
	private SourceLocation endPosition;
	
	public SourceRegion(SourceLocation startLoc, SourceLocation endLoc) {
		super();
		this.startPosition = startLoc;
		this.endPosition = endLoc;
	}
	
	public SourceLocation getStartPosition() {
		return startPosition;
	}

	public void setStartPosition(SourceLocation startPosition) {
		this.startPosition = startPosition;
	}

	public SourceLocation getEndPosition() {
		return endPosition;
	}

	public void setEndPosition(SourceLocation endPosition) {
		this.endPosition = endPosition;
	}

	public String toString() {
		if (startPosition instanceof Unknown || endPosition instanceof Unknown) {
			return " " + startPosition + " - " + endPosition;
		} else {
           TextSource startTextSrc = (TextSource) startPosition;
           TextSource endTextSrc = (TextSource) endPosition;
           int startLineNum = startTextSrc.getTextSourceLineNo();
           int startColNum = startTextSrc.getTextSourceCoNo();
           int endLineNum = endTextSrc.getTextSourceLineNo();
           int endColNum = endTextSrc.getTextSourceCoNo();
           if (startLineNum == endLineNum) {
        	   if (startColNum == endColNum) {
        		   return " " + startPosition;
        	   } else {
        		   return " @ " + startLineNum + " : " + startColNum + " - " + endColNum; 
        	   }
           } else {
        	   return " @ " + startLineNum + " : " + startColNum + " - " + endLineNum + " : " + endColNum;
           }
		}
	}
	
	public boolean equals(SourceRegion srcRegionObj) { 
		if(this.startPosition.equals(srcRegionObj.getStartPosition()) &&
		   this.endPosition.equals(srcRegionObj.getEndPosition())) return true;
		else return false;
	}
}

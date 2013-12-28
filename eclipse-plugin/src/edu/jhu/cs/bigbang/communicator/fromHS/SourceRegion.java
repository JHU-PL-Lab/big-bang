package edu.jhu.cs.bigbang.communicator.fromHS;

public class SourceRegion {
	
	private SourceLocation startPosition;
	private SourceLocation endPosition;
	
	public SourceRegion(SourceLocation startLoc, SourceLocation endLoc) {
		super();
		this.startPosition = startLoc;
		this.endPosition = endLoc;
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
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class BMLexFailure extends BatchModeError {
    
	private String errMsg;

    public String getErrMsg() {
	    return errMsg;
    }

    public void setErrMsg(String errMsg) {
	    this.errMsg = errMsg;
    }

    public BMLexFailure(int cmdId, String errMsg) {
	    super(cmdId);
	    this.errMsg = errMsg;
    }
    
    @Override
    public String toString() {
    	return errMsg;
    }
    
    public boolean equals(Object obj) {
    	if(obj == null) return false;
    	if(!(obj instanceof BMLexFailure)) return false;
    	BMLexFailure BMLexFailureObj = (BMLexFailure) obj;
    	return this.errMsg.equals(BMLexFailureObj.getErrMsg()); 
    }
    
}

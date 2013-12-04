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
    
}

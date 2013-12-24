package edu.jhu.cs.bigbang.communicator.fromHS;

public class BMProtocolFailure extends BatchModeError {
    
	private String errMsg;
	
	public String getErrMsg() {
		return errMsg;
	}
	
	public void setErrMsg(String errMsg) {
		this.errMsg = errMsg;
	}
	
	public BMProtocolFailure(int cmdId, String errMsg) {
		super(cmdId);
		this.errMsg = errMsg;
	}
	
}

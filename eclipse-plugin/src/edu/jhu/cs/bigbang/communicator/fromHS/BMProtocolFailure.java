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
    
	@Override
	public String toString() {
		return errMsg;		
	}
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
    	if(!(obj instanceof BMProtocolFailure)) return false;
    	BMProtocolFailure BMProtocolFailureObj = (BMProtocolFailure) obj;
    	return this.errMsg.equals(BMProtocolFailureObj.getErrMsg());
	}
}

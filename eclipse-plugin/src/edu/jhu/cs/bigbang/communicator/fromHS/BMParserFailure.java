package edu.jhu.cs.bigbang.communicator.fromHS;

public class BMParserFailure extends BatchModeError {

	private String errMsg;

	public String getErrMsg() {
		return errMsg;
	}

	public void setErrMsg(String errMsg) {
		this.errMsg = errMsg;
	}

	public BMParserFailure(int cmdId, String errMsg) {
		super(cmdId);
		this.errMsg = errMsg;
	}
	
	@Override
	public String toString() {
    	return errMsg;
    }
	
	public boolean equals(Object obj) {
		if(obj == null) return false;
    	if(!(obj instanceof BMParserFailure)) return false;
    	BMParserFailure BMParseFailureObj = (BMParserFailure) obj;
    	return this.errMsg.equals(BMParseFailureObj.getErrMsg());
	}
}

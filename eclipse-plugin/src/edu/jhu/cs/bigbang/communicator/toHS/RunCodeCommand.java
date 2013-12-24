package edu.jhu.cs.bigbang.communicator.toHS;

public class RunCodeCommand extends ToHaskellObject{
	private String usrInpStr;

	public String getUsrInpStr() {
		return usrInpStr;
	}

	public void setUsrInpStr(String usrInpStr) {
		this.usrInpStr = usrInpStr;
	}

	public RunCodeCommand(int cmdId, String usrInpStr) {
		super(cmdId);
		this.usrInpStr = usrInpStr;
	}

}

package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.communicator.util.CommunicatorSerializable;

public class FromHaskellObject implements CommunicatorSerializable{
	private int cmdId;

	public int getCmdId() {
		return cmdId;
	}

	public void setCmdId(int cmdId) {
		this.cmdId = cmdId;
	}

	public FromHaskellObject(int cmdId) {
		this.cmdId = cmdId;
	}
	
	


}

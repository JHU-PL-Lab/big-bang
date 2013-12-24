package edu.jhu.cs.bigbang.communicator.toHS;

import edu.jhu.cs.bigbang.communicator.util.CommunicatorSerializable;

public class ToHaskellObject implements CommunicatorSerializable {

	private int cmdId;

	public int getCmdId() {
		return cmdId;
	}

	public void setCmdId(int cmdId) {
		this.cmdId = cmdId;
	}

	public ToHaskellObject(int cmdId) {
		this.cmdId = cmdId;
	}

}

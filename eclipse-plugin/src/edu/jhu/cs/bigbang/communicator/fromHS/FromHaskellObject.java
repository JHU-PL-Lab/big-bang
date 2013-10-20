package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.communicator.util.CommunicatorSerializable;

public class FromHaskellObject implements CommunicatorSerializable{
	private int fho;

	public int getFho() {
		return fho;
	}

	public void setFho(int fho) {
		this.fho = fho;
	}

	public FromHaskellObject(int fho) {
		super();
		this.fho = fho;
	}
}

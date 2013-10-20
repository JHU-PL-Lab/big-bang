package edu.jhu.cs.bigbang.communicator.toHS;

import edu.jhu.cs.bigbang.communicator.util.CommunicatorSerializable;

public class ToHaskellObject implements CommunicatorSerializable{
	private int tho;

	public int getTho() {
		return tho;
	}

	public void setTho(int tho) {
		this.tho = tho;
	}

	public ToHaskellObject(int tho) {
		super();
		this.tho = tho;
	} 
}

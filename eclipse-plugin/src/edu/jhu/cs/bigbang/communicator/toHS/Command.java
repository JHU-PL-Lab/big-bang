package edu.jhu.cs.bigbang.communicator.toHS;

public class Command extends ToHaskellObject{
	private int c;

	public int getC() {
		return c;
	}

	public void setC(int c) {
		this.c = c;
	}

	public Command(int tho, int c) {
		super(tho);
		this.c = c;
	}
	
}

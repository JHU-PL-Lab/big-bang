package edu.jhu.cs.bigbang.communicator.fromHS;

public class RunCodeResponse extends Response{
	private int rcr;

	public int getRcr() {
		return rcr;
	}

	public void setRcr(int rcr) {
		this.rcr = rcr;
	}

	public RunCodeResponse(int fho, int r, int rcr) {
		super(fho, r);
		this.rcr = rcr;
	}
}

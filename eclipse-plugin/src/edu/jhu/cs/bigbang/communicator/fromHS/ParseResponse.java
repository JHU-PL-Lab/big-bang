package edu.jhu.cs.bigbang.communicator.fromHS;

public class ParseResponse extends Response{
	private int pr;

	public int getPr() {
		return pr;
	}

	public void setPr(int pr) {
		this.pr = pr;
	}

	public ParseResponse(int fho, int r, int pr) {
		super(fho, r);
		this.pr = pr;
	}
}

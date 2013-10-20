package edu.jhu.cs.bigbang.communicator.fromHS;

public class Response extends FromHaskellObject{
	private int r;

	public int getR() {
		return r;
	}

	public void setR(int r) {
		this.r = r;
	}

	public Response(int fho, int r) {
		super(fho);
		this.r = r;
	}
	
}

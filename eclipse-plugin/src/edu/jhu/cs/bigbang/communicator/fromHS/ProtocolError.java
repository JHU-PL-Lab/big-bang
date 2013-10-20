package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProtocolError extends FromHaskellObject{
	private int pe;

	public int getPe() {
		return pe;
	}

	public void setPe(int pe) {
		this.pe = pe;
	}

	public ProtocolError(int fho, int pe) {
		super(fho);
		this.pe = pe;
	}
}

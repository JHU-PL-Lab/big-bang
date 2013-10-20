package edu.jhu.cs.bigbang.communicator.toHS;

public class RunCodeCommand extends Command{
	private int rcc;

	public int getRcc() {
		return rcc;
	}

	public void setRcc(int rcc) {
		this.rcc = rcc;
	}

	public RunCodeCommand(int tho, int c, int rcc) {
		super(tho, c);
		this.rcc = rcc;
	}
	
}

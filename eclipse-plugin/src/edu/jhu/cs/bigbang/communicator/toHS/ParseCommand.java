package edu.jhu.cs.bigbang.communicator.toHS;

public class ParseCommand extends Command{
	private int pc;

	public int getPc() {
		return pc;
	}

	public void setPc(int pc) {
		this.pc = pc;
	}

	public ParseCommand(int tho, int c, int pc) {
		super(tho, c);
		this.pc = pc;
	}
}

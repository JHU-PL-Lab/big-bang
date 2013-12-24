package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class BatchModeError implements FromHaskellObject{
	
	private int cmdId;
    
	public int getCmdId() {
		return cmdId;
	}
    
	public void setCmdId(int cmdId) {
		this.cmdId = cmdId;
	}
    
	public BatchModeError(int cmdId) {
		super();
		this.cmdId = cmdId;
	}
	
}

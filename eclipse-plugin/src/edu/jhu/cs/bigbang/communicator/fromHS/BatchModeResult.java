package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.HashMap;

public class BatchModeResult implements FromHaskellObject{

	private int cmdId;
    private AbstractFlowVar flowVar;
	private HashMap<AbstractFlowVar, Value> flowToValueMap;
	private HashMap<AbstractCellVar, AbstractFlowVar> cellToFlowMap;			
    
	public int getCmdId() {
		return cmdId;
	}

	public void setCmdId(int cmdId) {
		this.cmdId = cmdId;
	}
	
	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public HashMap<AbstractFlowVar, Value> getFlowVarMap() {
		return flowToValueMap;
	}

	public void setFlowVarMap(HashMap<AbstractFlowVar, Value> flowVarMap) {
		this.flowToValueMap = flowVarMap;
	}

	public HashMap<AbstractCellVar, AbstractFlowVar> getCellVarMap() {
		return 	cellToFlowMap;
	}

	public void setCellVarMap(HashMap<AbstractCellVar, AbstractFlowVar> cellVarMap) {
		this.cellToFlowMap = cellVarMap;
	}
	
	public BatchModeResult(int cmdId, AbstractFlowVar flowVar,
			HashMap<AbstractFlowVar, Value> flowVarMap,
			HashMap<AbstractCellVar, AbstractFlowVar> cellVarMap) {
		super();
		this.cmdId = cmdId;
		this.flowVar = flowVar;
		this.flowToValueMap = flowVarMap;
		this.cellToFlowMap = cellVarMap;
	}

}

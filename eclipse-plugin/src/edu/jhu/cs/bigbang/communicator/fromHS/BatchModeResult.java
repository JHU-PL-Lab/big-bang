package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;


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
	
	public String toString() {
		String s = "Flow Var:" + flowVar.getFlowStr() + "\n";
		s += ":::::Flow to Value Map::::::" + "\n";
		Iterator it = flowToValueMap.entrySet().iterator();
	    while (it.hasNext()) {
	        Map.Entry<AbstractFlowVar, Value> pairs = (Map.Entry<AbstractFlowVar, Value>)it.next();
	        Value value = pairs.getValue();
//	        String sVal = null;
//	        if (value.getClass() == VInt.class) 
//	        	sVal = ((VInt)value).getIntVar() +"";	
//	        s += pairs.getKey().getFlowStr() + ", " + sVal + "\n";
	        
	    }
		s += "::::::Cell to Flow Map::::::" + "\n";
		Iterator it2 = cellToFlowMap.entrySet().iterator();
	    while (it2.hasNext()) {
	        Map.Entry<AbstractCellVar, AbstractFlowVar> pairs = 
	        		(Map.Entry<AbstractCellVar, AbstractFlowVar>)it2.next();
	        s += pairs.getKey().getCellVarStr() + ", " + pairs.getValue().getFlowStr() + "\n";
	    }		
		return s;
	}
	
}

package edu.jhu.cs.bigbang.eclipse.toploop;

import java.util.Map;

import edu.jhu.cs.bigbang.communicator.fromHS.AbstractCellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.BatchModeResult;
import edu.jhu.cs.bigbang.communicator.fromHS.Value;

public class ValueEnvironment {

	private Map<AbstractFlowVar, Value> flowVarMap;
	private Map<AbstractCellVar, AbstractFlowVar> cellVarMap;

	public ValueEnvironment(Map<AbstractFlowVar, Value> flowVarMap,
			Map<AbstractCellVar, AbstractFlowVar> cellVarMap) {
		this.flowVarMap = flowVarMap;
		this.cellVarMap = cellVarMap;
	}

	public ValueEnvironment(BatchModeResult result) {
		this.flowVarMap = result.getFlowVarMap();
		this.cellVarMap = result.getCellVarMap();
	}

	public String displayStringOf(AbstractFlowVar x) {
		return flowVarMap.get(x).toDisplayString(this);
	}

	public String displayStringOf(AbstractCellVar y) {
		return displayStringOf(cellVarMap.get(y));
	}
}
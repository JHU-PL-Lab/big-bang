package edu.jhu.cs.bigbang.eclipse.toploop;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import edu.jhu.cs.bigbang.communicator.fromHS.AbstractCellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.BatchModeResult;
import edu.jhu.cs.bigbang.communicator.fromHS.FromHaskellObject;
import edu.jhu.cs.bigbang.communicator.fromHS.VChar;
import edu.jhu.cs.bigbang.communicator.fromHS.VEmptyOnion;
import edu.jhu.cs.bigbang.communicator.fromHS.VInt;
import edu.jhu.cs.bigbang.communicator.fromHS.VLabel;
import edu.jhu.cs.bigbang.communicator.fromHS.Value;

public class DisplayObject {

	private String output;

	public DisplayObject(FromHaskellObject hasKellObject) {
		if (hasKellObject instanceof BatchModeResult) {
			BatchModeResult result = (BatchModeResult) hasKellObject;
			output = "";
			Object obj = getMappedValueOfFlowVar(result.getFlowVarMap(), result.getFlowVar());
			if(isDisplayableValue(obj))
				output = obj.toString();
			while (!isDisplayableValue(obj)) {
				obj = extract(result, obj);
			}
		} else {
			output = "error";
		}
	}

	public Object extract(BatchModeResult result, Object obj) {
		if (obj instanceof AbstractFlowVar) {
			obj = getMappedValueOfFlowVar(result.getFlowVarMap(), (AbstractFlowVar)obj);
			if(isDisplayableValue(obj))
				output += obj;
		} else if (obj instanceof VLabel) {
			VLabel label = (VLabel) obj;
			output += "`" + label.getLabelName() + " ";
			obj = getMappedFlowvarOfCell(result.getCellVarMap(), label.getCellVar());
		}
		return obj;
	}
	
	public String toString() {
		return output;
	}

	// This is a hacking method. There's no overridden version of 'equals'
	// method in AbstractFlowVar yet. That 'equals' method should check if the
	// content and origin of two flowvar are the same or not. I am waiting for
	// Chao to work on it. In a mean while, this method will compare only the
	// flow
	// content but not the origin. So this is just for testing and the result is
	// not always correct if there's 2 variables with the same name.
	public Value getMappedValueOfFlowVar(
			HashMap<AbstractFlowVar, Value> flowVarMap, AbstractFlowVar flowVar) {
		Iterator<Entry<AbstractFlowVar, Value>> it = flowVarMap.entrySet()
				.iterator();
		while (it.hasNext()) {
			Map.Entry<AbstractFlowVar, Value> pair = (Map.Entry<AbstractFlowVar, Value>) it
					.next();
			if (pair.getKey().getFlowStr().equals(flowVar.getFlowStr()))
				return pair.getValue();
		}
		return null;
	}

	// This is also a hacking method similar to the one above.
	public AbstractFlowVar getMappedFlowvarOfCell(
			HashMap<AbstractCellVar, AbstractFlowVar> cellMap,
			AbstractCellVar cellVar) {
		Iterator<Entry<AbstractCellVar, AbstractFlowVar>> it = cellMap
				.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<AbstractCellVar, AbstractFlowVar> pair = (Map.Entry<AbstractCellVar, AbstractFlowVar>) it
					.next();
			if (pair.getKey().getCellVarStr().equals(cellVar.getCellVarStr()))
				return pair.getValue();
		}
		return null;
	}

	public boolean isDisplayableValue(Object v) {
		if (v instanceof VInt || v instanceof VChar || v instanceof VEmptyOnion)
			return true;
		return false;

	}

}

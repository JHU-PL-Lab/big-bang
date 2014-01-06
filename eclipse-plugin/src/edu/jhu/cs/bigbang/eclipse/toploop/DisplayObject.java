package edu.jhu.cs.bigbang.eclipse.toploop;

import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.BatchModeResult;
import edu.jhu.cs.bigbang.communicator.fromHS.FromHaskellObject;

public class DisplayObject {

	private String output;

	public DisplayObject(FromHaskellObject hasKellObject) {
		if (hasKellObject instanceof BatchModeResult) {
			BatchModeResult result = (BatchModeResult) hasKellObject;
			ValueEnvironment env = new ValueEnvironment(result);
			AbstractFlowVar flowVar = result.getFlowVar();
			output = result.getFlowVarMap().get(flowVar).toDisplayString(env);
		} else {
			output = "Evaluation Error";
		}
	}

	public String toString() {
		return output;
	}
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public class SomeFlowVar extends AnyVar{
	
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public SomeFlowVar(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}

	public String toString() {
		return " " + flowVar;
	}
	
	public boolean equals(SomeFlowVar someFlowVarObj) {
		if(this.flowVar.equals(someFlowVarObj.getFlowVar())) {
			return true;
		}else {
			return false;
		}
	}
	
}

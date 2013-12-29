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

	@Override
	public String toString() {
		return " " + flowVar;
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		if (! (obj instanceof SomeFlowVar)) return false;		
		SomeFlowVar someFlowVarObj = (SomeFlowVar) obj;
		if(this.flowVar.equals(someFlowVarObj.getFlowVar())) {
			return true;
		}else {
			return false;
		}
	}
	
}

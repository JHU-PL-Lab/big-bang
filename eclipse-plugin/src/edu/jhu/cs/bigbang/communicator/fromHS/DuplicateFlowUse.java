package edu.jhu.cs.bigbang.communicator.fromHS;

public class DuplicateFlowUse extends IllFormedness {
	
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public DuplicateFlowUse(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}
	
	@Override
	public String toString() {
    	return " " + flowVar;
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		if (! (obj instanceof DuplicateFlowUse)) return false;
		DuplicateFlowUse duplicateFlowUseObj = (DuplicateFlowUse) obj;
		return (this.flowVar.equals(duplicateFlowUseObj.getFlowVar()));		
	}
}

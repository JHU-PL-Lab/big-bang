package edu.jhu.cs.bigbang.communicator.fromHS;

public class DuplicateFlowBinding extends IllFormedness{
	
	private AbstractFlowVar flowVar;

	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}

	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}

	public DuplicateFlowBinding(AbstractFlowVar flowVar) {
		super();
		this.flowVar = flowVar;
	}
	
	@Override
	public String toString() {
    	return " " + flowVar;
    }
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		if (! (obj instanceof DuplicateFlowBinding)) return false;
		DuplicateFlowBinding duplicateFlowBindingObj = (DuplicateFlowBinding) obj;
		return (this.flowVar.equals(duplicateFlowBindingObj.getFlowVar()));		
	}
	
}

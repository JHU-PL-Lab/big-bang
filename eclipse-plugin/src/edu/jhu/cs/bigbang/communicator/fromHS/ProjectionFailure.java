package edu.jhu.cs.bigbang.communicator.fromHS;

public class ProjectionFailure extends EvalError{
	
	private AbstractFlowVar flowVar;
	private AnyProjector anyProjector;
	
	public AbstractFlowVar getFlowVar() {
		return flowVar;
	}
	
	public void setFlowVar(AbstractFlowVar flowVar) {
		this.flowVar = flowVar;
	}
	
	public AnyProjector getAnyProject() {
		return anyProjector;
	}
	
	public void setAnyProject(AnyProjector anyProject) {
		this.anyProjector = anyProject;
	}
	
	public ProjectionFailure(AbstractFlowVar flowVar, AnyProjector anyProject) {
		super();
		this.flowVar = flowVar;
		this.anyProjector = anyProject;
	}
	
	@Override
	public String toString() {
		return " " + flowVar + " " + anyProjector;
	}
	
	public boolean equals(Object obj) { 
		if (obj == null) return false;
		if (! (obj instanceof ProjectionFailure)) return false;
		ProjectionFailure projectionFailureObj = (ProjectionFailure) obj;
		if (this.flowVar.equals(projectionFailureObj.getFlowVar()) &&
			this.anyProjector.equals(projectionFailureObj.getAnyProject())) return true;
		else return false;
	}
	
}

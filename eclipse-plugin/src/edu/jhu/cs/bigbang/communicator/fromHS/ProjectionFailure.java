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
}

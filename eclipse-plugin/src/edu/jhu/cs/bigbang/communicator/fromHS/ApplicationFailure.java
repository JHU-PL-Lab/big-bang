package edu.jhu.cs.bigbang.communicator.fromHS;

public class ApplicationFailure extends EvalError{
	private AbstractFlowVar flowVar1;
	private AbstractFlowVar flowVar2;
	
	public AbstractFlowVar getFlowVar1() {
		return flowVar1;
	}
	
	public void setFlowVar1(AbstractFlowVar flowVar1) {
		this.flowVar1 = flowVar1;
	}
	
	public AbstractFlowVar getFlowVar2() {
		return flowVar2;
	}
	
	public void setFlowVar2(AbstractFlowVar flowVar2) {
		this.flowVar2 = flowVar2;
	}
	
	public ApplicationFailure(AbstractFlowVar flowVar1, AbstractFlowVar flowVar2) {
		super();
		this.flowVar1 = flowVar1;
		this.flowVar2 = flowVar2;
	}
	
}

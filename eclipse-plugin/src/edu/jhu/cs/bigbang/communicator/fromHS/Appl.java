package edu.jhu.cs.bigbang.communicator.fromHS;

public class Appl extends Redex{
	
	private AbstractFlowVar flowVar2;

	public AbstractFlowVar getFlowVar2() {
		return flowVar2;
	}
	
	public void setFlowVar2(AbstractFlowVar flowVar2) {
		this.flowVar2 = flowVar2;
	}
	
	public Appl(Origin origin, AbstractFlowVar flowVar1,
			AbstractFlowVar flowVar2) {
		super(origin, flowVar1);		
		this.flowVar2 = flowVar2;
	}
	
}

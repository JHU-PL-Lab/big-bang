package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.eclipse.toploop.ValueEnvironment;

public class VOnion extends Value {

	private Origin origin;
	private AbstractFlowVar aFlowVar_1;
	private AbstractFlowVar aFlowVar_2;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public AbstractFlowVar getaFlowVar_1() {
		return aFlowVar_1;
	}

	public void setaFlowVar_1(AbstractFlowVar aFlowVar_1) {
		this.aFlowVar_1 = aFlowVar_1;
	}

	public AbstractFlowVar getaFlowVar_2() {
		return aFlowVar_2;
	}

	public void setaFlowVar_2(AbstractFlowVar aFlowVar_2) {
		this.aFlowVar_2 = aFlowVar_2;
	}

	public VOnion(Origin origin, AbstractFlowVar aFlowVar_1,
			AbstractFlowVar aFlowVar_2) {
		super();
		this.origin = origin;
		this.aFlowVar_1 = aFlowVar_1;
		this.aFlowVar_2 = aFlowVar_2;
	}

	@Override
	public String toString() {
		return aFlowVar_1 + " & " + aFlowVar_2;
	}

	public String toDisplayString(ValueEnvironment env) {
		return " (" + env.displayStringOf(aFlowVar_1) + ") & ("
				+ env.displayStringOf(aFlowVar_2) + ")";
	}

	public boolean equals(Object obj) {
		if (obj == null)
			return false;
		if (!(obj instanceof VOnion))
			return false;
		VOnion vOnionObj = (VOnion) obj;
		if (this.aFlowVar_1.equals(aFlowVar_1.getOrigin())
				&& this.aFlowVar_2.equals(vOnionObj.getaFlowVar_2())) {
			return true;
		} else {
			return false;
		}
	}

}
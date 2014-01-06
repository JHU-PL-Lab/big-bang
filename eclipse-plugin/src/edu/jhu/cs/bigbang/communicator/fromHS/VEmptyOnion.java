package edu.jhu.cs.bigbang.communicator.fromHS;

import edu.jhu.cs.bigbang.eclipse.toploop.ValueEnvironment;

public class VEmptyOnion extends Value{
	private Origin origin;

	public Origin getOrigin() {
		return origin;
	}

	public void setOrigin(Origin origin) {
		this.origin = origin;
	}

	public VEmptyOnion(Origin origin) {
		super();
		this.origin = origin;
	}
	
	@Override
	public String toString() {
		return "()";
	}

	public String toDisplayString(ValueEnvironment env) {
		return "()";
	}
	
	public boolean equals(Object obj) {
		if (obj == null) return false;
		else if (! (obj instanceof VEmptyOnion)) return false;
		else return true;
	}
}

package edu.jhu.cs.bigbang.communicator.exception;

public class TinyBangDeserializationInternalFailureException extends RuntimeException {
	private TinyBangProtocolException inner;

	public TinyBangDeserializationInternalFailureException(
			TinyBangProtocolException inner) {
		super();
		this.inner = inner;
	}

	public TinyBangProtocolException getInner() {
		return inner;
	}
	
}

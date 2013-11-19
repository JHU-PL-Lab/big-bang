package edu.jhu.cs.bigbang.communicator.exception;

public class TinyBangUnrecognizedDeserializationTypeException extends TinyBangProtocolException{

	public TinyBangUnrecognizedDeserializationTypeException(String typeName) {
		super("Unrecognized object type while deserializing: " + typeName);
		// TODO Auto-generated constructor stub
	}

}

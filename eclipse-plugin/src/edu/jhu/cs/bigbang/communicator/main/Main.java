package edu.jhu.cs.bigbang.communicator.main;


import edu.jhu.cs.bigbang.communicator.exception.TinyBangInternalErrorException;
import edu.jhu.cs.bigbang.communicator.exception.TinyBangProtocolException;
import edu.jhu.cs.bigbang.communicator.fromHS.EvalError;
import edu.jhu.cs.bigbang.communicator.fromHS.FromHaskellObject;
import edu.jhu.cs.bigbang.communicator.util.TinyBangRuntime;

public class Main {

	public static void main(String[] args) throws TinyBangProtocolException, TinyBangInternalErrorException {		
		TinyBangRuntime tbr = new TinyBangRuntime();
		FromHaskellObject fho = tbr.runSubProcess(); 
	}	
	
}

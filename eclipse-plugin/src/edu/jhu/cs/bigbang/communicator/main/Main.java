package edu.jhu.cs.bigbang.communicator.main;

import java.io.File;
import java.io.IOException;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;

import edu.jhu.cs.bigbang.communicator.exception.TinyBangInternalErrorException;
import edu.jhu.cs.bigbang.communicator.exception.TinyBangProtocolException;
import edu.jhu.cs.bigbang.communicator.toHS.*;
import edu.jhu.cs.bigbang.communicator.util.TinyBangRuntime;
import edu.jhu.cs.bigbang.communicator.util.ToHaskellObjectAdapter;

public class Main {

	public static void main(String[] args) throws TinyBangProtocolException, TinyBangInternalErrorException {		
		TinyBangRuntime tbr = new TinyBangRuntime();
		String test = tbr.runSubProcess(); 
		System.out.println(test);
	}	
	
}

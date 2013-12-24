package edu.jhu.cs.bigbang.communicator.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import edu.jhu.cs.bigbang.communicator.fromHS.FromHaskellObject;
import edu.jhu.cs.bigbang.communicator.toHS.*;
import edu.jhu.cs.bigbang.communicator.util.adapter.FromHaskellObjectAdapter;
import edu.jhu.cs.bigbang.communicator.util.adapter.ToHaskellObjectAdapter;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class TinyBangProcess {
	
	private Process p;
	private OutputStream stToHaskell = null;
	private InputStream stFromHaskell = null;
	private InputStream stderr = null;			
	private BufferedReader br = null;
	
	public TinyBangProcess(ProcessBuilder pb) {
		try {
			p = pb.start();
			stToHaskell = p.getOutputStream();
			stFromHaskell = p.getInputStream();
			br = new BufferedReader(new InputStreamReader(stFromHaskell));
			stderr = p.getErrorStream();
		} catch (IOException e) {		
			printf("Encount IOException, when tring to get stdin, stdout or stderr from subprocess");
		}
	}
	
	public void sendObj(ToHaskellObject tho) {
		
		GsonBuilder gb = new GsonBuilder();
		Gson g = gb.registerTypeHierarchyAdapter(ToHaskellObject.class, new ToHaskellObjectAdapter()).create();
		//Gson g = gb.create();
		String inputStr = g.toJson(tho);
		printf("Json String which will be sent to haskell: " + inputStr);
		BufferedReader br = new BufferedReader(new InputStreamReader(stFromHaskell));				
		
		try {
			
			stToHaskell.write((inputStr.trim() + "\n").getBytes());
			stToHaskell.close();
			
		   // TODO: something with the error stream
	       
			BufferedReader br_err = new BufferedReader(new InputStreamReader(stderr));			
			String errMsg = null;
			
			while ((errMsg=br_err.readLine()) != null) {
				System.out.println(errMsg);
			}
		    			
		} catch (IOException e) {		
			printf("Encount IOException, when tring to write json string to interpreter stdin");
		}			
	}
	
	// method for test
	public FromHaskellObject readObject() {
		
		String resultStr = null;
		FromHaskellObject fho = null;
		
		try {
			resultStr = br.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		GsonBuilder fhoGb = new GsonBuilder();
		fhoGb.registerTypeHierarchyAdapter(FromHaskellObject.class, new FromHaskellObjectAdapter());
		Gson fhoG = fhoGb.create();
		fho = fhoG.fromJson(resultStr, FromHaskellObject.class);		
		
		return fho;
		
	}
	
/*	public <T extends CommunicatorSerializable> T readObject(Class<T> clazz) {

		String resultStr = null;
		FromHaskellObject fko = null;				
		
		try {
			// get the right json format for gson 
			resultStr = br.readLine();
			printf("Json string received from haskell: " + resultStr);			
			
			GsonBuilder gb = new GsonBuilder();
	 		//gb.registerTypeHierarchyAdapter(FromHaskellObject.class, new FromHaskellObjectAdapter());
	 		Gson g = gb.create();	 		
			fko = g.fromJson(resultStr, FromHaskellObject.class);						
			
		} catch (IOException e2) {
			printf("Encount IOException when trying to read stdout.");
		}	
		
    
		if (fko instanceof ProtocolError) {
			throw new TinyBangProtocolException("Encount a protocol error.");
		} else if (fko instanceof FromHaskellObject) {			
            // The above isInstance check ensures that the following cast is safe
            @SuppressWarnings("...")
            T ret = (T)fko;
            return ret;
        } else {            
        	throw new TinyBangInternalErrorException("Encount an internal error.");									            
        }
	}
*/
	
	
	
	public void destroySubProcess() {
		p.destroy();
	}
	
	// util
	
	public void printf(Object obj) {
		System.out.println(obj);
	}
	
	public void print(Object obj) {
		System.out.print(obj);
	}
}

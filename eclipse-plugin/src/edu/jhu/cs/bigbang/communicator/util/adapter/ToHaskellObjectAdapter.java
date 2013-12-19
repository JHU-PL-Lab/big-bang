package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import edu.jhu.cs.bigbang.communicator.toHS.*;

public class ToHaskellObjectAdapter implements JsonSerializer<ToHaskellObject>{

	@Override
	public JsonElement serialize(ToHaskellObject src, Type typeOfSrc,
			JsonSerializationContext context) {
	
		JsonObject runCodeCmdJsonStr = new JsonObject();
	    String className = src.getClass().getSimpleName();
	    
	    runCodeCmdJsonStr.addProperty("type", className);
	    runCodeCmdJsonStr.addProperty("usrInpStr", ((RunCodeCommand)src).getUsrInpStr());
	    JsonObject superClass = new JsonObject();
	    // generate sub json node which represents the super class
	    superClass.addProperty("cmdId", 1);
	    superClass.addProperty("type", "Command");
	    runCodeCmdJsonStr.add("super", superClass);
	    	    
		return runCodeCmdJsonStr;
	}	
	
}

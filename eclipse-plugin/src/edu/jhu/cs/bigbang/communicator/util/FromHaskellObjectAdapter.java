package edu.jhu.cs.bigbang.communicator.util;

import java.lang.reflect.Type;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import edu.jhu.cs.bigbang.communicator.fromHS.*;


public class FromHaskellObjectAdapter implements JsonDeserializer<FromHaskellObject> {

	@Override
	public FromHaskellObject deserialize(JsonElement src, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {

		return null;
	}
     
	/*@Override

	@Override
	public FromHaskellObject deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		String objType = jo.get("objectType").getAsString();
		int fho = jo.get("fho").getAsInt();
		if(objType.equals("RunCodeResponse")) {
			int r = jo.get("r").getAsInt();
			int rcr = jo.get("rcr").getAsInt();
			return new RunCodeResponse(fho, r, rcr);
		} else if (objType.equals("ParseResponse")){
			int r = jo.get("r").getAsInt();
			int pr = jo.get("pr").getAsInt();
			return new ParseResponse(fho, r, pr);
		} else {
			int pe = jo.get("pe").getAsInt();
			return new ProtocolError(fho, pe);
		}
	}*/
}

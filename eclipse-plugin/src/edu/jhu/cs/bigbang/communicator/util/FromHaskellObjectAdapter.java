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
import edu.jhu.cs.bigbang.communicator.toHS.RunCodeCommand;

public class FromHaskellObjectAdapter implements
		JsonSerializer<FromHaskellObject>, JsonDeserializer<FromHaskellObject> {

	@Override
	// implement this method for symmetry
	public JsonElement serialize(FromHaskellObject src, Type typeOfSrc,
			JsonSerializationContext context) {

		JsonObject jo = new JsonObject();
		String className = src.getClass().getSimpleName();

		if (className.equals("ParseResponse")) {
			jo.addProperty("objectType", className);
			jo.addProperty("fho", src.getFho());
			jo.addProperty("r", ((Response) src).getR());
			jo.addProperty("pr", ((ParseResponse) src).getPr());
			return jo;
		} else if (className.equals("RunCodeResponse")) {
			jo.addProperty("objectType", className);
			jo.addProperty("fho", src.getFho());
			jo.addProperty("r", ((Response) src).getR());
			jo.addProperty("rcr", ((RunCodeResponse) src).getRcr());
			return jo;
		} else {
			jo.addProperty("objectType", className);
			jo.addProperty("fho", src.getFho());
			jo.addProperty("pe", ((ProtocolError) src).getPe());
			return jo;
		}
	}

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
	}
}

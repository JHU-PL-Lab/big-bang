package edu.jhu.cs.bigbang.communicator.util;

import java.lang.reflect.Type;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import edu.jhu.cs.bigbang.communicator.toHS.*;

public class ToHaskellObjectAdapter implements JsonSerializer<ToHaskellObject>, JsonDeserializer<ToHaskellObject>{

	/*
	 * SPI services provider interface (possible design pattern can be applied in the future)
	private static Map<String,Deserializer> deserializers = new HashMap<>();
	
	public static void registerDeserializer(String name, Class<? extends Deserializer> clazz)
	{
		deserializers.put(name, clazz.newInstance());
		//deserializers.put("RunCodeCommand",new RunCodeCommandDeserializer());
	}
	 */
	
	// this method is implemented for symmetry 
	public ToHaskellObject deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		String objType = jo.get("objectType").getAsString();
		int tho = jo.get("tho").getAsInt();
		/*
		ObjectDeserializer d = deserializers.get(objType);
		return d.deserialize(jo);
		 */
		if(objType.equals("RunCodeCommand")) {
			int c = jo.get("c").getAsInt();
			int rcc = jo.get("rcc").getAsInt();
			return new RunCodeCommand(tho, c, rcc);
		} else {
			int c = jo.get("c").getAsInt();
			int pc = jo.get("pc").getAsInt();
			return new ParseCommand(tho, c, pc);
		}				
	}

	public JsonElement serialize(ToHaskellObject src, Type typeOfSrc,
			JsonSerializationContext context) {
		
		JsonObject result = new JsonObject(); 
		String className = src.getClass().getSimpleName();
		if (className.equals("RunCodeCommand")) {
			result.addProperty("objectType", className);
			result.addProperty("tho", src.getTho());
			result.addProperty("c", ((Command) src).getC());
			result.addProperty("rcc", ((RunCodeCommand) src).getRcc());
			return result;
		} else {
			result.addProperty("objectType", className);
			result.addProperty("tho", src.getTho());
			result.addProperty("c", ((Command) src).getC());
			result.addProperty("pc", ((ParseCommand) src).getPc());
			return result;
		}
				
	}
	
}

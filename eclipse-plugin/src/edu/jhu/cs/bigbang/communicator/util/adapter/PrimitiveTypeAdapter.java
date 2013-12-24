package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.PrimChar;
import edu.jhu.cs.bigbang.communicator.fromHS.PrimInt;
import edu.jhu.cs.bigbang.communicator.fromHS.PrimitiveType;

public class PrimitiveTypeAdapter implements JsonDeserializer<PrimitiveType>{

	@Override
	public PrimitiveType deserialize(JsonElement je, Type typeOfObj,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		String type = jo.get("type").getAsString();
		
		PrimitiveType pt = null;
		
		if(type.equals("PrimInt")) {
			pt = new PrimInt(origin);
		}else {
			pt = new PrimChar(origin);
		}
		
		return pt;
	}
	

}

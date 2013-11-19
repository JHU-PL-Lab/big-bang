package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.OnionOp;
import edu.jhu.cs.bigbang.communicator.fromHS.OpOnionProj;
import edu.jhu.cs.bigbang.communicator.fromHS.OpOnionSub;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class OnionOpAdapter implements JsonDeserializer<OnionOp>{

	@Override
	public OnionOp deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		String type = jo.get("type").getAsString();
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(),Origin.class);
		
		OnionOp oo = null;
		
		if (type == "OpOnionSub") {
			oo = new OpOnionSub(origin);
		} else {
			oo = new OpOnionProj(origin);
		}
		
		return oo;
	}
	

}

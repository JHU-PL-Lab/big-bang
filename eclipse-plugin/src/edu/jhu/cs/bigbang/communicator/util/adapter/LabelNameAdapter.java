package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.LabelName;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class LabelNameAdapter implements JsonDeserializer<LabelName>{

	@Override
	public LabelName deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		GsonBuilder gb = new GsonBuilder();
		gb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson g = gb.create();
		Origin origin = g.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		String nameStr = jo.get("nameStr").getAsString();
		LabelName lName = new LabelName(origin,nameStr);
		
		return lName;
	}

}

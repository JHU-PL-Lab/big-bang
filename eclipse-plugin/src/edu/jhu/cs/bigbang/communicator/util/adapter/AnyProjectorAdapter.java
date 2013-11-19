package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.AnyProjector;
import edu.jhu.cs.bigbang.communicator.fromHS.Projector;

public class AnyProjectorAdapter implements JsonDeserializer<AnyProjector>{

	@Override
	public AnyProjector deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		GsonBuilder projectorGb = new GsonBuilder();
		projectorGb.registerTypeHierarchyAdapter(Projector.class, new ProjectorAdapter());
		Gson projectorG = projectorGb.create();		
		
		Projector projector = projectorG.fromJson(jo.get("projector").getAsJsonObject(), Projector.class);
		
		AnyProjector anyProjector = new AnyProjector(projector);
		
		return anyProjector;
	}
	

}

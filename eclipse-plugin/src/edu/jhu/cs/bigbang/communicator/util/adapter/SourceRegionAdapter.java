package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.SourceLocation;
import edu.jhu.cs.bigbang.communicator.fromHS.SourceRegion;

public class SourceRegionAdapter implements JsonDeserializer<SourceRegion>{

	@Override
	public SourceRegion deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
        
		GsonBuilder locGb = new GsonBuilder();
        locGb.registerTypeHierarchyAdapter(SourceLocation.class, new SourceLocationAdapter());
        Gson locG = locGb.create();
        
        JsonObject jo = (JsonObject) je;
        
        SourceLocation startLocation = locG.fromJson(jo.get("startPosition").getAsJsonObject(), SourceLocation.class); 
        SourceLocation endLocation = locG.fromJson(jo.get("endPosition").getAsJsonObject(), SourceLocation.class); 
        
        SourceRegion sr = new SourceRegion(startLocation, endLocation);
        
		return sr;
		
	}

}

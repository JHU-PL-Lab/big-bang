package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.CellQualifier;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.QualFinal;
import edu.jhu.cs.bigbang.communicator.fromHS.QualImmutable;
import edu.jhu.cs.bigbang.communicator.fromHS.QualNone;

public class CellQualifierAdapter implements JsonDeserializer<CellQualifier>{

	@Override
	public CellQualifier deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		String  type = jo.get("type").getAsString();
		
		CellQualifier cq = null;
		
		if(type == "QualFinal") {
			
			cq = new QualFinal(origin);
			
		} else if (type == "QualImmutable") {
			
			cq = new QualImmutable(origin);
			
		} else {
			
			cq = new QualNone(origin);
			
		}
		
		return cq;
	}

}

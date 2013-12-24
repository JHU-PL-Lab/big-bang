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
import edu.jhu.cs.bigbang.communicator.fromHS.PrimitiveType;
import edu.jhu.cs.bigbang.communicator.fromHS.ProjFun;
import edu.jhu.cs.bigbang.communicator.fromHS.ProjLabel;
import edu.jhu.cs.bigbang.communicator.fromHS.ProjPrim;
import edu.jhu.cs.bigbang.communicator.fromHS.Projector;

public class ProjectorAdapter implements JsonDeserializer<Projector>{

	@Override
	public Projector deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
        GsonBuilder originGb = new GsonBuilder();
        originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
        Gson originG = originGb.create();
        
        Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
        
        Projector projector = null;
        
        String type = jo.get("type").getAsString();
        if(type.equals("ProjPrim")) {
        	
        	GsonBuilder primitiveTypeGb = new GsonBuilder();
        	primitiveTypeGb.registerTypeHierarchyAdapter(PrimitiveType.class, new PrimitiveTypeAdapter());
        	Gson primitiveTypeG = primitiveTypeGb.create();
        	
        	PrimitiveType primitiveType = primitiveTypeG.fromJson(jo.get("primitiveType").getAsJsonObject(), PrimitiveType.class);
        	
        	projector = new ProjPrim(origin, primitiveType);
        	
        }else if (type.equals("ProjLabel")) {
        	
        	GsonBuilder labelNameGb = new GsonBuilder();
        	labelNameGb.registerTypeHierarchyAdapter(LabelName.class, new LabelNameAdapter());
        	Gson labelNameG = labelNameGb.create();
        	
        	LabelName labelName = labelNameG.fromJson(jo.get("labelName").getAsJsonObject(), LabelName.class);
        	
        	projector = new ProjLabel(origin, labelName);
        	
        }else {
        	
        	projector = new ProjFun(origin);
        	
        }
        
		return projector;
	}

}

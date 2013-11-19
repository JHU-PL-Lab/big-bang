package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.VOnion;

public class VOnionAdapter implements JsonDeserializer<VOnion>{

	@Override
	public VOnion deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
		Gson flowVarG = flowVarGb.create();
		
		AbstractFlowVar aFlowVar_1 = flowVarG.fromJson(jo.get("flowVar_1").getAsJsonObject(), AbstractFlowVar.class);
		AbstractFlowVar aFlowVar_2 = flowVarG.fromJson(jo.get("flowVar_2").getAsJsonObject(), AbstractFlowVar.class);
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		VOnion vo = new VOnion(origin, aFlowVar_1, aFlowVar_2);
		
		return vo;
	}
	

}

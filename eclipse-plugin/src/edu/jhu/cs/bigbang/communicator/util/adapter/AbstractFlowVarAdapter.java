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
import edu.jhu.cs.bigbang.communicator.fromHS.FlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.GenFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class AbstractFlowVarAdapter implements JsonDeserializer<AbstractFlowVar>{

	@Override
	public AbstractFlowVar deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		String type = jo.get("type").getAsString();
		AbstractFlowVar aFlowVar = null;
		GsonBuilder gb = new GsonBuilder();
		gb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson g = gb.create();
		Origin origin = g.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		String flowContents = jo.get("flowContents").getAsString();
		if(type=="FlowVar") {						
			aFlowVar = new FlowVar(origin, flowContents);
		} else {
			int flowNum = jo.get("flowNum").getAsInt();
			aFlowVar = new GenFlowVar(origin, flowContents, flowNum);
		}
		
		return aFlowVar;
	}

}

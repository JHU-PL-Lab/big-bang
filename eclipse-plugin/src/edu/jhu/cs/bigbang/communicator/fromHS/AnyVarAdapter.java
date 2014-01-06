package edu.jhu.cs.bigbang.communicator.fromHS;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.util.adapter.AbstractCellVarAdapter;
import edu.jhu.cs.bigbang.communicator.util.adapter.AbstractFlowVarAdapter;

public class AnyVarAdapter implements JsonDeserializer<AnyVar>{

	@Override
	public AnyVar deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
        
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
		Gson flowVarG = flowVarGb.create();
		
		GsonBuilder cellVarGb = new GsonBuilder();
		cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
		Gson cellVarG = cellVarGb.create();
		
		String type = jo.get("type").getAsString();
		AnyVar anyVar = null;
		
		if (type.equals("SomeFlowVar")) {
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			anyVar = new SomeFlowVar(flowVar);
		} else {
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			anyVar = new SomeCellVar(cellVar);
		}
		
		
		return anyVar;
	}
	
}

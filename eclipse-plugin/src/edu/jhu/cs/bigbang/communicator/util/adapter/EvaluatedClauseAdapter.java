package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.AbstractCellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.CellDef;
import edu.jhu.cs.bigbang.communicator.fromHS.CellQualifier;
import edu.jhu.cs.bigbang.communicator.fromHS.CellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.EvaluatedClause;
import edu.jhu.cs.bigbang.communicator.fromHS.Flow;
import edu.jhu.cs.bigbang.communicator.fromHS.FlowKind;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.Value;
import edu.jhu.cs.bigbang.communicator.fromHS.ValueDef;

public class EvaluatedClauseAdapter implements JsonDeserializer<EvaluatedClause>{

	@Override
	public EvaluatedClause deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {

		JsonObject jo = (JsonObject) je;
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
		Gson flowVarG = flowVarGb.create();
		
		GsonBuilder cellVarGb = new GsonBuilder();
		cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
		Gson cellVarG = cellVarGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		String type = jo.get("type").getAsString();
		
		EvaluatedClause ec = null;
		
		if (type.equals("ValueDef")) {

			GsonBuilder valueGb = new GsonBuilder();
			valueGb.registerTypeHierarchyAdapter(Value.class, new ValueAdapter());
			Gson valueG = valueGb.create();
			
			Value value = valueG.fromJson(jo.get("value").getAsJsonObject(), Value.class);
			
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);					
			
			ec = new ValueDef(origin, flowVar, value);
			
		} else if (type.equals("CellDef")) {
	
			GsonBuilder cellQualifierGb = new GsonBuilder();
			cellQualifierGb.registerTypeHierarchyAdapter(CellQualifier.class, new CellQualifierAdapter());
			Gson cellQualifierG = cellQualifierGb.create();
			
			CellQualifier cellQualifier = cellQualifierG.fromJson(jo.get("cellQualifier").getAsJsonObject(), CellQualifier.class);
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			
			ec = new CellDef(origin, cellQualifier, flowVar, cellVar);
			
		} else {
			
			GsonBuilder generalGb = new GsonBuilder();
			Gson generalG = generalGb.create();
			FlowKind flowKind = generalG.fromJson(jo.get("flowKind").getAsString(), FlowKind.class);
			
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AbstractFlowVar flowVar2 = flowVarG.fromJson(jo.get("flowVar2").getAsJsonObject(), AbstractFlowVar.class);
			
			ec = new Flow(origin, flowVar, flowKind, flowVar2);
			
		}
				
		return ec;
	}
	
}
 
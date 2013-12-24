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
import edu.jhu.cs.bigbang.communicator.fromHS.CellGet;
import edu.jhu.cs.bigbang.communicator.fromHS.CellSet;
import edu.jhu.cs.bigbang.communicator.fromHS.Clause;
import edu.jhu.cs.bigbang.communicator.fromHS.Evaluated;
import edu.jhu.cs.bigbang.communicator.fromHS.EvaluatedClause;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.Redex;
import edu.jhu.cs.bigbang.communicator.fromHS.RedexDef;
import edu.jhu.cs.bigbang.communicator.fromHS.Throws;

public class ClauseAdapter implements JsonDeserializer<Clause>{

	@Override
	public Clause deserialize(JsonElement je, Type typeOfSrc,
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
		
		GsonBuilder redexGb = new GsonBuilder();
		redexGb.registerTypeHierarchyAdapter(Redex.class, new RedexAdapter());
		Gson redexG = redexGb.create();
		
		GsonBuilder evaluatedClauseGb = new GsonBuilder();
		evaluatedClauseGb.registerTypeHierarchyAdapter(EvaluatedClause.class, new EvaluatedClauseAdapter());
		Gson evaluatedClauseG = evaluatedClauseGb.create();
		
		String type = jo.get("type").getAsString();
		
		Clause clause = null;
		
		if(type.equals("RedexDef")) {
			
			Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			Redex redex = redexG.fromJson(jo.get("redex").getAsJsonObject(), Redex.class);
			
			clause = new RedexDef(origin, flowVar, redex);
			
		} else if (type.equals("CellSet")) {
			
			Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			
			clause = new CellSet(origin, cellVar, flowVar);
			
		} else if (type.equals("CellGet")) {
			
			Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			
			clause = new CellGet(origin, flowVar, cellVar);
			
		} else if (type.equals("Throws")) {
			
			Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AbstractFlowVar flowVar2 = flowVarG.fromJson(jo.get("flowVar2").getAsJsonObject(), AbstractFlowVar.class);
			
			clause = new Throws(origin, flowVar, flowVar2);
			
		} else {
			
			EvaluatedClause evaluatedClause = evaluatedClauseG.fromJson(jo.get("evaluatedClause").getAsJsonObject(), EvaluatedClause.class);
			
			clause = new Evaluated(evaluatedClause);
			
		}
		
		return clause;
	}
	
}

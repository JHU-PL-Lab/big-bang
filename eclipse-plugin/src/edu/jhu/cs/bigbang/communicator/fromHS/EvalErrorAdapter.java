package edu.jhu.cs.bigbang.communicator.fromHS;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Iterator;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.util.adapter.AbstractCellVarAdapter;
import edu.jhu.cs.bigbang.communicator.util.adapter.AbstractFlowVarAdapter;
import edu.jhu.cs.bigbang.communicator.util.adapter.AnyProjectorAdapter;

public class EvalErrorAdapter implements JsonDeserializer<EvalError>{

	@Override
	public EvalError deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder illFormednessGb = new GsonBuilder();
		illFormednessGb.registerTypeHierarchyAdapter(IllFormedness.class, new IllFormednessAdapter());
		Gson illFormednessG = illFormednessGb.create();
		
		GsonBuilder anyVarGb = new GsonBuilder();
		anyVarGb.registerTypeHierarchyAdapter(AnyVar.class, new AnyVarAdapter());
		Gson anyVarG = anyVarGb.create();
		
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
		Gson flowVarG = flowVarGb.create(); 
		
		GsonBuilder cellVarGb = new GsonBuilder();
		cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
		Gson cellVarG = cellVarGb.create();
		
		GsonBuilder anyProjectGb = new GsonBuilder();
		anyProjectGb.registerTypeHierarchyAdapter(AnyProjector.class, new AnyProjectorAdapter());
		Gson anyProjectG = anyProjectGb.create();
		
		EvalError evalError = null;
		
		String type = jo.get("type").getAsString();
		
		if (type.equals("IllFormedExpression")) {
			
			IllFormedness illFormedness = illFormednessG.fromJson(jo.get("illFormedness").getAsJsonObject(), IllFormedness.class);
			evalError = new IllFormedExpression(illFormedness);
			
		} else if (type.equals("OpenExpression")) {
			
			ArrayList<AnyVar> anyVarLst = new ArrayList<AnyVar>(); 
		    JsonArray jsonArray = jo.get("VarSet").getAsJsonArray();
		    Iterator<JsonElement> i = jsonArray.iterator();
		    
		    while (i.hasNext()) {
		    	anyVarLst.add(anyVarG.fromJson(i.next(), AnyVar.class));
		    }
			
		    evalError = new OpenExpression(anyVarLst);
		    
		} else if (type.equals("FlowVarNotClosed")) {
			
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			evalError = new FlowVarNotClosed(flowVar);
			
			
		} else if (type.equals("CellVarNotClosed")) {
			
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			evalError = new CellVarNotClosed(cellVar);
			
		} else if (type.equals("ProjectionFailure")) {
			
			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AnyProjector anyProjector = anyProjectG.fromJson(jo.get("anyProjector"), AnyProjector.class);
			evalError = new ProjectionFailure(flowVar, anyProjector);
			
		} else {
			
			AbstractFlowVar flowVar1 = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			AbstractFlowVar flowVar2 = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
			evalError =new ApplicationFailure(flowVar1, flowVar2);
			
		}
		
		return evalError;
	}

}

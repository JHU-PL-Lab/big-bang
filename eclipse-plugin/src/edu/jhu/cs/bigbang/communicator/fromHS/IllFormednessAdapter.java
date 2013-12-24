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
import edu.jhu.cs.bigbang.communicator.util.adapter.ClauseAdapter;

public class IllFormednessAdapter implements JsonDeserializer<IllFormedness>{

	@Override
	public IllFormedness deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
	    Gson flowVarG = flowVarGb.create();
		
	    GsonBuilder cellVarGb = new GsonBuilder();
	    cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
	    Gson cellVarG = cellVarGb.create();
	    
	    GsonBuilder clauseGb = new GsonBuilder();
	    clauseGb.registerTypeHierarchyAdapter(Clause.class, new ClauseAdapter());
	    Gson clauseG = clauseGb.create();
	    
	    String type = jo.get("type").getAsString();
	    IllFormedness ifn = null;
	    
	    if (type.equals("DuplicateFlowBinding")) {
	    	
	    	AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
	        ifn = new DuplicateFlowBinding(flowVar);
	    		    	
	    } else if (type.equals("DuplicateFlowUse")) {
	    	
	    	AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
	    	ifn = new DuplicateFlowBinding(flowVar);
	    	
	    } else if (type.equals("DuplicateCellBinding")) {
	    	
	    	AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
	    	ifn = new DuplicateCellBinding(cellVar);
	    	
	    } else if (type.equals("InvalidExpressionEnd")) {
	    	
	    	Clause clause = clauseG.fromJson(jo.get("Clause").getAsJsonObject(), Clause.class);
	    	ifn = new InvalidExpressionEnd(clause);
	    	
	    } else {
	    	
	    	String content = "EmptyExpression";
	    	ifn = new EmptyExpression(content);
	    	
	    }
	    	    
		return ifn;
	}

}

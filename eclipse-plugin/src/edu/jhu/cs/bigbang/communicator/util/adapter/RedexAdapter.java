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
import edu.jhu.cs.bigbang.communicator.fromHS.Appl;
import edu.jhu.cs.bigbang.communicator.fromHS.BinOp;
import edu.jhu.cs.bigbang.communicator.fromHS.BinaryOperator;
import edu.jhu.cs.bigbang.communicator.fromHS.Define;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.Redex;

public class RedexAdapter implements JsonDeserializer<Redex>{

	@Override
	public Redex deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		GsonBuilder flowVarGb = new GsonBuilder();
		flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class, new AbstractFlowVarAdapter());
		Gson flowVarG = flowVarGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar").getAsJsonObject(), AbstractFlowVar.class);
		
		String type = jo.get("type").getAsString();
		
		Redex redex = null;
		
		if (type == "Define") {
			
			redex = new Define(origin, flowVar);
			
		} else if (type == "Appl") {
			
			AbstractFlowVar flowVar2 = flowVarG.fromJson(jo.get("flowVar2").getAsJsonObject(), AbstractFlowVar.class);
			redex = new Appl(origin, flowVar, flowVar2);
			
		} else {
			
			GsonBuilder binaryOperatorGb = new GsonBuilder();
			binaryOperatorGb.registerTypeHierarchyAdapter(BinaryOperator.class, new BinaryOperatorAdapter());
			Gson binaryOperatorG = binaryOperatorGb.create();
			
			BinaryOperator binaryOperator = binaryOperatorG.fromJson(jo.get("binaryOperator").getAsJsonObject(), BinaryOperator.class);
			
			AbstractFlowVar flowVar2 = flowVarG.fromJson(jo.get("flowVar2").getAsJsonObject(), AbstractFlowVar.class);
			
			redex = new BinOp(origin, flowVar, binaryOperator, flowVar2);
			
		}
		
		return redex;
	}

}

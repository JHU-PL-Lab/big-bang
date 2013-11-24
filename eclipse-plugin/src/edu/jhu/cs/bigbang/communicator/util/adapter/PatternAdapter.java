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
import edu.jhu.cs.bigbang.communicator.fromHS.ExnPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.InnerPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.Pattern;
import edu.jhu.cs.bigbang.communicator.fromHS.ValuePattern;

public class PatternAdapter implements JsonDeserializer<Pattern>{

	@Override
	public Pattern deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		String type = jo.get("type").getAsString();
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		GsonBuilder cellVarGb = new GsonBuilder();
		cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
		Gson cellVarG = cellVarGb.create();
		
		AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
		
		GsonBuilder innerPatternGb = new GsonBuilder();
		innerPatternGb.registerTypeHierarchyAdapter(InnerPattern.class, new InnerPatternAdapter());
		Gson innerPatternG = innerPatternGb.create();
		
		InnerPattern innerPattern = innerPatternG.fromJson(jo.get("innerPattern").getAsJsonObject(), InnerPattern.class);
		
		Pattern pattern= null;
		
		if (type.equals("ValuePattern")) {
			pattern = new ValuePattern(origin, cellVar, innerPattern);
		} else {
			pattern = new ExnPattern(origin, cellVar, innerPattern);
		}
		
		return pattern;
	} 

}

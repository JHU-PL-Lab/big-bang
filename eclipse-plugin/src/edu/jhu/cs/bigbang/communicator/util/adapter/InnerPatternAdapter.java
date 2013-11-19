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
import edu.jhu.cs.bigbang.communicator.fromHS.ConjunctionPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.EmptyOnionPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.InnerPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.LabelName;
import edu.jhu.cs.bigbang.communicator.fromHS.LabelPattern;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.PrimitivePattern;
import edu.jhu.cs.bigbang.communicator.fromHS.PrimitiveType;
import edu.jhu.cs.bigbang.communicator.fromHS.ScapePattern;

public class InnerPatternAdapter implements JsonDeserializer<InnerPattern>{

	@Override
	public InnerPattern deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		String type = jo.get("type").getAsString();
		
		InnerPattern innerPattern = null;
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		if (type == "PrimitivePattern") {
			
			GsonBuilder primitiveTypeGb = new GsonBuilder();
			primitiveTypeGb.registerTypeHierarchyAdapter(PrimitiveType.class, new PrimitiveTypeAdapter());
			Gson primitiveG = primitiveTypeGb.create();
			
			PrimitiveType primitiveTyep = primitiveG.fromJson(jo.get("primitiveType").getAsJsonObject(), PrimitiveType.class);
			
			innerPattern = new PrimitivePattern(origin, primitiveTyep);
						
		} else if (type == "LabelPattern") {
			
			GsonBuilder labelNameGb= new GsonBuilder();
			labelNameGb.registerTypeHierarchyAdapter(LabelName.class, new LabelNameAdapter());
			Gson labelNameG = labelNameGb.create();
			
			LabelName labelName = labelNameG.fromJson(jo.get("labelName").getAsJsonObject(), LabelName.class);
			
			GsonBuilder cellVarGb = new GsonBuilder();
			cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
			Gson cellVarG = cellVarGb.create();
			
			AbstractCellVar cellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
			
			GsonBuilder innerPatternGb = new GsonBuilder();
			innerPatternGb.registerTypeHierarchyAdapter(InnerPattern.class, new InnerPatternAdapter());
			Gson innerPatternG = innerPatternGb.create();
			
			InnerPattern innerPatternVar = innerPatternG.fromJson(jo.get("innerPattern").getAsJsonObject(), InnerPattern.class);
			
			innerPattern = new LabelPattern(origin, labelName, cellVar, innerPatternVar);
			
		} else if (type == "ConjunctionPattern") {
			
			GsonBuilder innerPatternGb = new GsonBuilder();
			innerPatternGb.registerTypeHierarchyAdapter(InnerPattern.class, new InnerPatternAdapter());
			Gson innerPatternG = innerPatternGb.create();
			
			InnerPattern innerPattern1 = innerPatternG.fromJson(jo.get("innerPattern").getAsJsonObject(), InnerPattern.class);
			InnerPattern innerPattern2 = innerPatternG.fromJson(jo.get("innerPattern2").getAsJsonObject(), InnerPattern.class);
			
			innerPattern = new ConjunctionPattern(origin, innerPattern1, innerPattern2);
		} else if (type == "ScapePattern") {
			
			innerPattern = new ScapePattern(origin);
			
		} else {
			
			innerPattern = new EmptyOnionPattern(origin);
			
		}
		
		return innerPattern;
	}
	

}

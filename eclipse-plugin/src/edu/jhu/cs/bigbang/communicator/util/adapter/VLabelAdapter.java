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
import edu.jhu.cs.bigbang.communicator.fromHS.LabelName;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.VLabel;

public class VLabelAdapter implements JsonDeserializer<VLabel>{

	@Override
	public VLabel deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		GsonBuilder labelNameGb = new GsonBuilder();
		labelNameGb.registerTypeHierarchyAdapter(LabelName.class, new LabelNameAdapter());
		Gson labelNameG = labelNameGb.create();
		
		GsonBuilder cellVarGb = new GsonBuilder();
		cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class, new AbstractCellVarAdapter());
		Gson cellVarG = cellVarGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		LabelName labelName = labelNameG.fromJson(jo.get("labelName").getAsJsonObject(), LabelName.class); 
		
		AbstractCellVar abstractCellVar = cellVarG.fromJson(jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);
		
		VLabel vl = new VLabel(origin, labelName, abstractCellVar);
		
		return vl;
	}

}
	
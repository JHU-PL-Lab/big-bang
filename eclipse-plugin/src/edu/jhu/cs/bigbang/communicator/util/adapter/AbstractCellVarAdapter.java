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
import edu.jhu.cs.bigbang.communicator.fromHS.CellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.GenCellVar;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class AbstractCellVarAdapter implements JsonDeserializer<AbstractCellVar>{

	@Override
	public AbstractCellVar deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		JsonObject jo = (JsonObject) je;
		AbstractCellVar aCellVar = null;				
		String type = jo.get("type").getAsString();
		GsonBuilder gb = new GsonBuilder();
		gb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson g = gb.create();
		Origin origin = g.fromJson(jo.get("origin").getAsJsonObject(),Origin.class);
		String cellContents = jo.get("cellContents").getAsString();
		if (type.equals("CellVar")) {
			aCellVar = new CellVar(origin, cellContents);
		}else {
			int cellNum = jo.get("cellNum").getAsInt();
			aCellVar = new GenCellVar(origin, cellContents, cellNum);
		}
		return aCellVar;
	}

}

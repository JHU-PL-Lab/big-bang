package edu.jhu.cs.bigbang.communicator.util.adapter;

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

import edu.jhu.cs.bigbang.communicator.fromHS.Clause;
import edu.jhu.cs.bigbang.communicator.fromHS.Expr;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class ExprAdapter implements JsonDeserializer<Expr>{

	@Override
	public Expr deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
		
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		
		GsonBuilder clauseGb = new GsonBuilder();
		clauseGb.registerTypeHierarchyAdapter(Clause.class, new ClauseAdapter());
		Gson clauseG = clauseGb.create();
		
		ArrayList<Clause> clauseLst = new ArrayList<Clause>();
		
		JsonArray jsonArray = jo.get("clauseLst").getAsJsonArray();
		
		Iterator<JsonElement> i = jsonArray.iterator();
		
		while(i.hasNext()) {
			clauseLst.add(clauseG.fromJson((JsonObject)i.next(), Clause.class));
		}
		
		Expr expr = new Expr(origin, clauseLst);
		
		return expr;
	}

}

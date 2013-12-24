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

import edu.jhu.cs.bigbang.communicator.fromHS.ComputedOrigin;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.SourceOrigin;
import edu.jhu.cs.bigbang.communicator.fromHS.SourceRegion;

public class OriginAdapter implements JsonDeserializer<Origin>{

	@Override
	public Origin deserialize(JsonElement src, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
        
		JsonObject jo = (JsonObject) src;
        String type = jo.get("type").getAsString();
        Origin origin = null;
        
        if(type.equals("SourceOrigin")) {
        	
        	// deserialize json string for SourceOrigin
        	GsonBuilder sourceRegionGb = new GsonBuilder();
        	sourceRegionGb.registerTypeHierarchyAdapter(SourceRegion.class, new SourceRegionAdapter());
        	Gson sourceRegionG = sourceRegionGb.create();
        	origin = new SourceOrigin(sourceRegionG.fromJson(jo.get("SourceOrigin").getAsJsonObject(), SourceRegion.class));
        	
        } else {
        	
        	// generate gson instance which adapt the origin Adapter
        	GsonBuilder gb = new GsonBuilder();
        	gb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
        	Gson g = gb.create();
        	// get the jsonArray
        	JsonArray jArr = jo.get("ComputedOrigin").getAsJsonArray();
        	Iterator i = jArr.iterator();
        	// ArrayList for construct computedOrigin
        	ArrayList<Origin> originLst = new ArrayList<Origin>();
        	JsonObject tmpJsonObj = new JsonObject();
        	// iterate the [origin] list
        	while (i.hasNext()) {
        		// generate origin object by using gson
        		tmpJsonObj = (JsonObject)i.next();
        		Origin eleOrigin = g.fromJson(tmpJsonObj, Origin.class);
        		originLst.add(eleOrigin);        		
        	}
        	
        	// generate computedOrigin object 
        	origin = new ComputedOrigin(originLst);
        	
        }
        
		return origin;
	}
	
}

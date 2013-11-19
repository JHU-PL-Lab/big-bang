package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.fromHS.SourceDocument;
import edu.jhu.cs.bigbang.communicator.fromHS.SourceLocation;
import edu.jhu.cs.bigbang.communicator.fromHS.TextSource;
import edu.jhu.cs.bigbang.communicator.fromHS.Unknown;

public class SourceLocationAdapter implements JsonDeserializer<SourceLocation>{

	@Override
	public SourceLocation deserialize(JsonElement src, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) src;
		String type = jo.get("type").getAsString();
		SourceLocation sl = null;
		GsonBuilder gb = new GsonBuilder();
		Gson g = gb.create();
		
		if(type == "TextSource") {
			
			SourceDocument srcDoc = g.fromJson(jo.get("textSourceDocument").getAsJsonObject(), SourceDocument.class);
			int textSourceLineNo = jo.get("textSourceLineNo").getAsInt();
			int textSourceColNo = jo.get("textSourceColNo").getAsInt();
			sl = new TextSource(srcDoc, textSourceLineNo, textSourceColNo);
			
		}else{
			
		    sl = new Unknown(jo.get("type").getAsString());
		    
		}
		
 		return sl;
 		
	}
	
}

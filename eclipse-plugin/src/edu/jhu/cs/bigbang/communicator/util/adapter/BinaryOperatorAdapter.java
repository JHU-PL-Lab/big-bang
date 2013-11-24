package edu.jhu.cs.bigbang.communicator.util.adapter;

import java.lang.reflect.Type;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

import edu.jhu.cs.bigbang.communicator.exception.TinyBangDeserializationInternalFailureException;
import edu.jhu.cs.bigbang.communicator.exception.TinyBangUnrecognizedDeserializationTypeException;
import edu.jhu.cs.bigbang.communicator.fromHS.BinaryOperator;
import edu.jhu.cs.bigbang.communicator.fromHS.OpEqual;
import edu.jhu.cs.bigbang.communicator.fromHS.OpGreater;
import edu.jhu.cs.bigbang.communicator.fromHS.OpLess;
import edu.jhu.cs.bigbang.communicator.fromHS.OpMinus;
import edu.jhu.cs.bigbang.communicator.fromHS.OpMult;
import edu.jhu.cs.bigbang.communicator.fromHS.OpPlus;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;

public class BinaryOperatorAdapter implements JsonDeserializer<BinaryOperator>{

	@Override
	public BinaryOperator deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {
		
		JsonObject jo = (JsonObject) je;
		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();
				
		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(), Origin.class);
		String type = jo.get("type").getAsString();
		BinaryOperator bo;
		
		// TODO enum
		
		if (type.equals("OpPlus")) {
			
			bo = new OpPlus(origin);
			
		} else if (type.equals("OpMinus")) {
			
			bo = new OpMinus(origin);
			
		} else if (type.equals("OpMult")) {
			
			bo = new OpMult(origin);
			
		} else if (type.equals("OpEqual")) {
			
			bo = new OpEqual(origin);
			
		} else if (type.equals("OpLess")) {
			
			bo = new OpLess(origin);
			
		} else if (type.equals("OpGreater")) {
			
			bo = new OpGreater(origin);
			
		} else {
			
			throw new TinyBangDeserializationInternalFailureException(new TinyBangUnrecognizedDeserializationTypeException(type));
			
		}
		
		return bo;
	}
	

}

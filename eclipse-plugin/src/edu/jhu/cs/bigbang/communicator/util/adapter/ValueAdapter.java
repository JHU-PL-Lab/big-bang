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
import edu.jhu.cs.bigbang.communicator.fromHS.AbstractFlowVar;
import edu.jhu.cs.bigbang.communicator.fromHS.AnyProjector;
import edu.jhu.cs.bigbang.communicator.fromHS.Expr;
import edu.jhu.cs.bigbang.communicator.fromHS.LabelName;
import edu.jhu.cs.bigbang.communicator.fromHS.OnionOp;
import edu.jhu.cs.bigbang.communicator.fromHS.Origin;
import edu.jhu.cs.bigbang.communicator.fromHS.Pattern;
import edu.jhu.cs.bigbang.communicator.fromHS.VChar;
import edu.jhu.cs.bigbang.communicator.fromHS.VEmptyOnion;
import edu.jhu.cs.bigbang.communicator.fromHS.VInt;
import edu.jhu.cs.bigbang.communicator.fromHS.VLabel;
import edu.jhu.cs.bigbang.communicator.fromHS.VOnion;
import edu.jhu.cs.bigbang.communicator.fromHS.VOnionFilter;
import edu.jhu.cs.bigbang.communicator.fromHS.VScape;
import edu.jhu.cs.bigbang.communicator.fromHS.Value;

public class ValueAdapter implements JsonDeserializer<Value> {

	@Override
	public Value deserialize(JsonElement je, Type typeOfSrc,
			JsonDeserializationContext context) throws JsonParseException {

		JsonObject jo = (JsonObject) je;
		String type = jo.get("type").getAsString();

		GsonBuilder originGb = new GsonBuilder();
		originGb.registerTypeHierarchyAdapter(Origin.class, new OriginAdapter());
		Gson originG = originGb.create();

		Origin origin = originG.fromJson(jo.get("origin").getAsJsonObject(),
				Origin.class);

		Value value = null;

		if (type.equals("VInt")) {

			int intVar = jo.get("intVar").getAsInt();
			value = new VInt(origin, intVar);

		} else if (type.equals("VChar")) {

			char charVar = jo.get("charVar").getAsCharacter();
			value = new VChar(origin, charVar);

		} else if (type.equals("VEmptyOnion")) {

			value = new VEmptyOnion(origin);

		} else if (type.equals("VLabel")) {

			GsonBuilder labelNameGb = new GsonBuilder();
			labelNameGb.registerTypeHierarchyAdapter(LabelName.class,
					new LabelNameAdapter());
			Gson labelNameG = labelNameGb.create();

			GsonBuilder cellVarGb = new GsonBuilder();
			cellVarGb.registerTypeHierarchyAdapter(AbstractCellVar.class,
					new AbstractCellVarAdapter());
			Gson cellVarG = cellVarGb.create();

			LabelName labelName = labelNameG.fromJson(jo.get("labelName")
					.getAsJsonObject(), LabelName.class);

			AbstractCellVar abstractCellVar = cellVarG.fromJson(
					jo.get("cellVar").getAsJsonObject(), AbstractCellVar.class);

			value = new VLabel(origin, labelName, abstractCellVar);

		} else if (type.equals("VOnion")) {

			GsonBuilder flowVarGb = new GsonBuilder();
			flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class,
					new AbstractFlowVarAdapter());
			Gson flowVarG = flowVarGb.create();

			AbstractFlowVar aFlowVar_1 = flowVarG.fromJson(jo.get("flowVar_1")
					.getAsJsonObject(), AbstractFlowVar.class);
			AbstractFlowVar aFlowVar_2 = flowVarG.fromJson(jo.get("flowVar_2")
					.getAsJsonObject(), AbstractFlowVar.class);

			value = new VOnion(origin, aFlowVar_1, aFlowVar_2);

		} else if (type.equals("VOnionFilter")) {

			GsonBuilder flowVarGb = new GsonBuilder();
			flowVarGb.registerTypeHierarchyAdapter(AbstractFlowVar.class,
					new AbstractFlowVarAdapter());
			Gson flowVarG = flowVarGb.create();

			AbstractFlowVar flowVar = flowVarG.fromJson(jo.get("flowVar")
					.getAsJsonObject(), AbstractFlowVar.class);

			GsonBuilder onionOpGb = new GsonBuilder();
			onionOpGb.registerTypeHierarchyAdapter(OnionOp.class,
					new OnionOpAdapter());
			Gson onionOpG = onionOpGb.create();

			OnionOp onionOp = onionOpG.fromJson(jo.get("onionOp")
					.getAsJsonObject(), OnionOp.class);

			GsonBuilder anyProjectorGb = new GsonBuilder();
			anyProjectorGb.registerTypeHierarchyAdapter(AnyProjector.class,
					new AnyProjectorAdapter());
			Gson anyProjectorG = anyProjectorGb.create();

			AnyProjector anyProjector = anyProjectorG.fromJson(
					jo.get("anyProjector").getAsJsonObject(),
					AnyProjector.class);

			value = new VOnionFilter(origin, flowVar, onionOp, anyProjector);

		} else if (type.equals("VScape")) {

			GsonBuilder patternGb = new GsonBuilder();
			patternGb.registerTypeHierarchyAdapter(Pattern.class,
					new PatternAdapter());
			Gson patternG = patternGb.create();

			GsonBuilder exprGb = new GsonBuilder();
			exprGb.registerTypeHierarchyAdapter(Expr.class, new ExprAdapter());
			Gson exprG = exprGb.create();

			Pattern pattern = patternG.fromJson(jo.get("pattern")
					.getAsJsonObject(), Pattern.class);
			Expr expr = exprG.fromJson(jo.get("expr").getAsJsonObject(),
					Expr.class);

			value = new VScape(origin, pattern, expr);

		} else {
			throw new IllegalStateException(
					"Received unknown object type from runtime: " + type);
		}

		return value;
	}

}

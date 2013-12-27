package edu.jhu.cs.bigbang.communicator.fromHS;

import java.util.ArrayList;
import java.util.Iterator;

public class BMEvalFailure extends BatchModeError{
	private EvalError evalError;
	private ArrayList<Clause> clauseLst;
	
	public EvalError getEvalError() {
		return evalError;
	}
	
	public void setEvalError(EvalError evalError) {
		this.evalError = evalError;
	}
	
	public ArrayList<Clause> getClauseLst() {
		return clauseLst;
	}
	
	public void setClauseLst(ArrayList<Clause> clauseLst) {
		this.clauseLst = clauseLst;
	}
	
	public BMEvalFailure(int cmdId, EvalError evalError,
			ArrayList<Clause> clauseLst) {
		super(cmdId);
		this.evalError = evalError;
		this.clauseLst = clauseLst;
	}
	
	public String toString() {
		StringBuffer resultStr = null;
		resultStr.append(evalError.toString());
		Iterator<Clause> arrLstI = clauseLst.iterator();
		while(arrLstI.hasNext()) {
			resultStr.append(arrLstI.next());
		}
		return resultStr.toString();
	}
	
}

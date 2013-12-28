package edu.jhu.cs.bigbang.communicator.fromHS;
import java.util.ArrayList;
import java.util.Iterator;

public class ComputedOrigin extends Origin{
	
	private ArrayList<Origin> originArr;

	public ArrayList<Origin> getOriginArr() {
		return originArr;
	}

	public void setOriginArr(ArrayList<Origin> originArr) {
		this.originArr = originArr;
	}

	public ComputedOrigin(ArrayList<Origin> originArr) {
		super();
		this.originArr = originArr;
	} 
	
	public String toString() {
		StringBuffer resultStr = null;
		resultStr.append("( computed from: ");
		Iterator<Origin> originArrI = originArr.iterator();
		while(originArrI.hasNext()) {
			resultStr.append(originArrI.next() + " ");
		}
		resultStr.append(")");
		return resultStr.toString();
	}
	
	public boolean equals(ComputedOrigin computedOrigObj) {
		if(this.originArr.equals(computedOrigObj.getOriginArr())) return true;
		else return false;
	}
}

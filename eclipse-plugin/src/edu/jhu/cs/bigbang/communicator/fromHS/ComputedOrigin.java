package edu.jhu.cs.bigbang.communicator.fromHS;
import java.util.ArrayList;

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
}

package edu.jhu.cs.bigbang.communicator.fromHS;

public abstract class AbstractCellVar {

	private Origin origin;
	private String cellContents;
	
	public Origin getOrigin() {
		return origin;
	}
	
	public void setOrigin(Origin origin) {
		this.origin = origin;
	}
	
	public String getCellVarStr() {
		return cellContents;
	}
	
	public void setCellVarStr(String cellVarStr) {
		this.cellContents= cellVarStr;
	}
	
	public AbstractCellVar(Origin origin, String cellVarStr) {
		super();
		this.origin = origin;
		this.cellContents= cellVarStr;
	}
	
	public String toString() {
		return cellContents;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((cellContents == null) ? 0 : cellContents.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractCellVar other = (AbstractCellVar) obj;
		if (cellContents == null) {
			if (other.cellContents != null)
				return false;
		} else if (!cellContents.equals(other.cellContents))
			return false;
		return true;
	}
	
 }

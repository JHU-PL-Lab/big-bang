package edu.jhu.cs.bigbang.communicator.fromHS;

public class GenCellVar extends AbstractCellVar {

	private int cellNum;

	public int getCellInt() {
		return cellNum;
	}

	public void setCellInt(int cellInt) {
		this.cellNum = cellInt;
	}

	public GenCellVar(Origin origin, String cellStr, int cellInt) {
		super(origin, cellStr);
		this.cellNum = cellInt;
	}

	@Override
	public String toString() {
		return " " + cellNum;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + cellNum;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		GenCellVar other = (GenCellVar) obj;
		if (cellNum != other.cellNum)
			return false;
		return true;
	}

}

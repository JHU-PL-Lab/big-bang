package edu.jhu.cs.bigbang.eclipse.editors.helper;

import org.eclipse.jface.text.formatter.IFormattingStrategy;

public class SpaceFormattingStrategy implements IFormattingStrategy {

	@Override
	public String format(String input, boolean arg1, String arg2, int[] arg3) {
		// /////////////////////////////////////////////////
		// Note - 11/11/2013
		// This is a dummy code just for demonstration.
		// 1. Trim all lines
		// 2. Make all 1+ whitespaces to be a white space
		String output = "";
		String[] split = input.split("\n");
		for (int i = 0; i < split.length; i++) {
			split[i] = split[i].trim().replaceAll(" +", " ");
			output += (split[i] + "\n");
		}
		// //////////////////////////////////////////////////
		return output;
	}

	@Override
	public void formatterStarts(String arg0) {
	}

	@Override
	public void formatterStops() {
	}

}

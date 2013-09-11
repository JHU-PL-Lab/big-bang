package edu.jhu.cs.bigbang.eclipse.process;

import java.io.File;
import java.io.IOException;

import edu.jhu.cs.bigbang.eclipse.Activator;

/**
 * A process manager to create and control processes
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class ProcessManager {

	/**
	 * Create and return a new process
	 * @return A new process object
	 */
	public Process startNewProcess() {
		
		String fullPath = Activator.getDefault().getInterpreterPath();
		int lastSlash = fullPath.lastIndexOf("/");
		String dir = fullPath.substring(0, lastSlash);
		String bin = "./"  + fullPath.substring(lastSlash + 1);
		
		ProcessBuilder builder = new ProcessBuilder();
		builder.directory(new File(dir));
		builder.command(bin);
		builder.redirectErrorStream(true);
		try {
			return builder.start();
		} catch (IOException e) {
			System.err.println("Error while starting a new process.");
			e.printStackTrace();
		}
		return null;
	}
	
}

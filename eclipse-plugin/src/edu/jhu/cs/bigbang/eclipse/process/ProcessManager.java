package edu.jhu.cs.bigbang.eclipse.process;

import java.io.File;
import java.io.IOException;

/**
 * A process manager to create and control processes
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class ProcessManager {

	//
	//
	// The path to the interpreter. Since we haven't defined the protocol yet. 
	// I just left it this way for now.
	//
	//
	public static final String BIGBANG_PATH = "/Users/GeE/BigBang/tiny-bang-interpreter/dist/build/interpreter";
	public static final String[] BIGBANG_CMD = new String[] { "./interpreter"};

	/**
	 * Create and return a new process
	 * @return A new process object
	 */
	public Process startNewProcess() {
		ProcessBuilder builder = new ProcessBuilder();
		builder.directory(new File(BIGBANG_PATH));
		builder.command(BIGBANG_CMD);
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

package edu.jhu.cs.bigbang.eclipse.toploop;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Observable;

import edu.jhu.cs.bigbang.communicator.exception.TinyBangInternalErrorException;
import edu.jhu.cs.bigbang.communicator.exception.TinyBangProtocolException;
import edu.jhu.cs.bigbang.communicator.fromHS.FromHaskellObject;
import edu.jhu.cs.bigbang.communicator.util.TinyBangRuntime;
import edu.jhu.cs.bigbang.eclipse.Activator;
import edu.jhu.cs.bigbang.eclipse.process.ProcessManager;

/**
 * A single TopLoop that can be accessed by the whole system.
 * (Now it is a singleton class. May be we need to change it later to support
 * multiple instances)
 * 
 * The topLoop is observed by TopLoopView object.
 * It will notify the TopLoopView when there is an update from
 * the interpreter.
 * 
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu> 
 *
 */
public class TopLoop extends Observable {

	public static final String OUTPUT_PREFIX = "# ";

	private static TopLoop instance;

	private TinyBangRuntime tbRuntime;

	// The buffer stores the output from the interpreter
	private StringBuffer returnBuffer;

	/**
	 * Get a globally shared instance
	 * @return A TopLoop instance
	 */
	public static TopLoop getInstance() {
		if (instance == null)
			instance = new TopLoop();
		return instance;
	}

	public static TopLoop getNewInstace() {
		instance = new TopLoop();
		return instance;
	}

	// A private constructor to create a TopLoop instance
	private TopLoop() {
		returnBuffer = new StringBuffer();
		tbRuntime = new TinyBangRuntime(Activator.getDefault().getInterpreterPath(), "--batch-mode");
		returnBuffer.append("Current Interpreter: "
				+ tbRuntime.getInterpreterPath());
	}

	/**
	 * Evaluate the given line
	 * @param s A line to be evaluated.
	 */
	public void eval(String s) {

		// We append the line to the returnBuffer first.
		// This will show the users what they typed.
		returnBuffer.append(s + "\n");
		FromHaskellObject returnObj = null;
		try {
			returnObj = tbRuntime.runSubProcess(s);
		} catch (TinyBangProtocolException e) {
			e.printStackTrace();
		} catch (TinyBangInternalErrorException e) {
			e.printStackTrace();
		}
		returnBuffer.append("> " + returnObj + "\n");
		setChanged();
		notifyObservers();
	}

	public void clear() {
		returnBuffer.setLength(0);
		returnBuffer.append("Current Interpreter: "
				+ Activator.getDefault().getInterpreterPath());
		returnBuffer.append("\n");
		setChanged();
		notifyObservers();
	}

	/**
	 * Get the content in the returnBuffer
	 * @return The content in returnBuffer
	 */
	public String getTopLoopString() {
		return returnBuffer.toString();
	}
}

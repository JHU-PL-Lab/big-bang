package edu.jhu.cs.bigbang.eclipse.toploop;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Observable;

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
public class TopLoop extends Observable implements Runnable {

	public static final String OUTPUT_PREFIX = "# ";

	private static TopLoop instance;

	private ProcessManager processManager;
	private Process currentProcess;
	private BufferedReader reader;
	private BufferedWriter writer;

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
		processManager = new ProcessManager();
		currentProcess = processManager.startNewProcess();
		returnBuffer.append("Current Interpreter: "
				+ Activator.getDefault().getInterpreterPath());
		try {
			reader = new BufferedReader(new InputStreamReader(
					currentProcess.getInputStream()));
			writer = new BufferedWriter(new OutputStreamWriter(
					currentProcess.getOutputStream()));
		} catch (Exception e) {
			System.err.println("TopLoop can't communicate with the process.");
		}
		// Start it self
		Thread readerThread = new Thread(this);
		readerThread.start();
	}

	/**
	 * Evaluate the given line
	 * @param s A line to be evaluated.
	 */
	public void eval(String s) {

		// We append the line to the returnBuffer first.
		// This will show the users what they typed.
		returnBuffer.append(s + "\n");
		try {
			writer.write(s);
			writer.flush();
		} catch (IOException e) {
			System.err.print("Error while writing to BigBang");
		}
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

	@Override
	public void run() {
		String line = null;
		try {
			while ((line = reader.readLine()) != null) {
				returnBuffer.append(line);
				returnBuffer.append("\n");
				setChanged();
				notifyObservers();
			}
		} catch (Exception e) {
			System.err.println("Error while reading from BigBang!");
		}
	}

}

package edu.jhu.cs.bigbang.eclipse.editors;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.NumberRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.IRule;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/**
 * A basic scanner for syntax coloring.
 * Right now we only hilight onion and numbers.
 * This scanner is a rule based and not supposed to handle semantic hiligting.
 * For semantic hilighting, please look at ().
 *  
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
class ColorScanner extends RuleBasedScanner {

	public static RGB ONION_RGB = new RGB(29, 109, 161);
	public static String ONION_START_SYMBOL = "`";
	public static String ONION_STOP_SYMBOL = " ";

	public static RGB NUMBER_RGB = new RGB(108, 170, 10);

	public ColorScanner() {

		Color onionColor = new Color(Display.getCurrent(), ONION_RGB);
		Color numberColor = new Color(Display.getCurrent(), NUMBER_RGB);
		Token onion = new Token(new TextAttribute(onionColor, null, SWT.BOLD));
		Token number = new Token(new TextAttribute(numberColor));
		setRules(new IRule[] {
				new NumberRule(number),
				new SingleLineRule(ONION_START_SYMBOL, ONION_STOP_SYMBOL, onion) });
	}

}
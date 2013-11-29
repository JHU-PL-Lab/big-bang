package edu.jhu.cs.bigbang.eclipse.toploop;

import java.util.Observable;
import java.util.Observer;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import edu.jhu.cs.bigbang.eclipse.Activator;
import edu.jhu.cs.bigbang.eclipse.util.ImageConstant;

/**
 * A Top loop GUI. Users can code in the input panel, 
 * press enter, and see the results in the output panel.
 *  
 * TopLoopView is an observer. It observes the state of the TopLoop object. 
 *  
 * @author Keeratipong Ukachoke <kukacho1@jhu.edu>
 *
 */
public class TopLoopView extends ViewPart implements Observer {

	public static final String ID = "edu.jhu.cs.bigbang.eclipse.toploop.TopLoopView";

	private static final int BOUND_OFFSET = 3;
	private static final double INPUT_RATIO_HEIGHT = 0.8;
	private static final int INPUT_MAX_HEIGHT = 25;
	private static final String DISPLAYED_NAME = "BigBang Top Loop";

	private Composite container;
	private StyledText inputPanel;
	private StyledText outputPanel;
	private Sash inputOutputSash;

	public void createPartControl(Composite parent) {
		this.setPartName(DISPLAYED_NAME);

		// The main containner
		container = new Composite(parent, SWT.BORDER);
		container.addControlListener(new ControlAdapter() {
			@Override
			public void controlResized(ControlEvent e) {
				resized();
			}
		});

		// Initialize the input panel
		inputPanel = new StyledText(this.container, SWT.V_SCROLL);
		inputPanel.setWordWrap(true);
		inputPanel.setFont(JFaceResources.getTextFont());
		inputPanel.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r' || e.character == '\n') {
					TopLoop.getInstance().eval(inputPanel.getText());
					outputPanel.append("# " + inputPanel.getText());
					inputPanel.setText("");
				}
			}
		});

		// Initialize the output panel
		outputPanel = new StyledText(this.container, SWT.V_SCROLL);
		outputPanel.setWordWrap(true);
		outputPanel.setFont(JFaceResources.getTextFont());
		outputPanel.setEditable(false);

		// Initialize a sash be tween input & output panel
		inputOutputSash = new Sash(container, SWT.HORIZONTAL | SWT.SMOOTH);
		inputOutputSash.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				inputOutputSash.setBounds(e.x, e.y, e.width, e.height);
				layout();
			}
		});
		TopLoop.getInstance().addObserver(this);
		Activator.getDefault().setTopLoopView(this);

		// Initailze buttons
		IActionBars actionBars = this.getViewSite().getActionBars();
		IToolBarManager toolBarManager = actionBars.getToolBarManager();

		ImageDescriptor iconClear = ImageConstant
				.getImageDescriptor(ImageConstant.CLEAR_ICON);
		Action actionClear = new Action("Clear", iconClear) {
			@Override
			public void run() {
				TopLoop.getInstance().clear();
			}
		};
		toolBarManager.add(actionClear);
	}

	public void bindToTopLoop(TopLoop toploop) {
		TopLoop.getInstance().addObserver(this);
	}

	protected void layout() {
		Rectangle parentArea = this.container.getClientArea();
		int width = parentArea.width;
		int height = parentArea.height;
		Rectangle sashBounds = inputOutputSash.getBounds();
		outputPanel.setBounds(0, 0, width, sashBounds.y);
		inputPanel.setBounds(0, sashBounds.y + BOUND_OFFSET, width, height
				- sashBounds.y - BOUND_OFFSET);
		inputOutputSash.setBounds(0, sashBounds.y, width, BOUND_OFFSET);
	}

	protected void resized() {
		Rectangle parentArea = this.container.getClientArea();
		int width = parentArea.width;
		int height = parentArea.height;
		int userTextHeight = (int) (height * (1 - INPUT_RATIO_HEIGHT));
		if (userTextHeight < INPUT_MAX_HEIGHT)
			userTextHeight = INPUT_MAX_HEIGHT;
		outputPanel.setBounds(0, 0, width, height - userTextHeight
				- BOUND_OFFSET);
		inputPanel.setBounds(0, height - userTextHeight, width, userTextHeight);
		inputOutputSash.setBounds(0, height - userTextHeight - BOUND_OFFSET,
				width, BOUND_OFFSET);
	}

	@Override
	public void update(Observable o, Object output) {
		if (outputPanel != null && !outputPanel.isDisposed()) {
			Display.getDefault().syncExec(new Runnable() {
				public void run() {
					outputPanel.setText(TopLoop.getInstance()
							.getTopLoopString());
					outputPanel.setSelection(outputPanel.getCharCount());
				}
			});
		}
	}

	@Override
	public void setFocus() {
	}

	public static void forceShowTopLoopView() {
		IWorkbenchPage activePage = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage();
		try {
			activePage.showView(TopLoopView.ID);
		} catch (PartInitException e) {
			System.err.println("Error while displaying the top loop window.");
			e.printStackTrace();
		}
	}

}
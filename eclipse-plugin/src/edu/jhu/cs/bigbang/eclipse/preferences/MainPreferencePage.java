package edu.jhu.cs.bigbang.eclipse.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import edu.jhu.cs.bigbang.eclipse.Activator;
import edu.jhu.cs.bigbang.eclipse.toploop.TopLoop;

public class MainPreferencePage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	private FileFieldEditor interpreterField;

	public MainPreferencePage() {
		super(FieldEditorPreferencePage.GRID);
		this.setPreferenceStore(Activator.getDefault().getPreferenceStore());
		this.setDescription("Bigbang Preferences");
	}

	@Override
	protected void createFieldEditors() {

		Group toolsGroup = new Group(getFieldEditorParent(), SWT.NONE);
		GridData data = new GridData(GridData.FILL, GridData.FILL, true, false);
		toolsGroup.setLayoutData(data);

		interpreterField = new FileFieldEditor(Preference.INTERPRETER_PATH,
				" Interpreter Path", toolsGroup);
		this.addField(interpreterField);

	}

	@Override
	public void init(IWorkbench arg0) {
	}

	@Override
	public boolean performOk() {
		boolean ok = super.performOk();
		Activator.getDefault().getTopLoopView().bindToTopLoop(TopLoop.getNewInstace());
		return ok;
	}

}

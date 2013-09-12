package edu.jhu.cs.bigbang.eclipse.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import edu.jhu.cs.bigbang.eclipse.toploop.TopLoopView;
import edu.jhu.cs.bigbang.eclipse.wizard.EmptyProjectWizard;
import edu.jhu.cs.bigbang.eclipse.wizard.NewFileWizard;

public class BigbangPerspective implements IPerspectiveFactory {

	public static String ID = "edu.jhu.cs.bigbang.eclipse.perspective.BigbangPerspective";
	
	@Override
	public void createInitialLayout(IPageLayout layout) {
		
		String editorArea = layout.getEditorArea();
		
		layout.addNewWizardShortcut(EmptyProjectWizard.ID);
		layout.addNewWizardShortcut(NewFileWizard.ID);
		
		layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		layout.addShowViewShortcut(TopLoopView.ID);
		
		IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, 0.25f, editorArea);
		left.addView(IPageLayout.ID_RES_NAV);
		
		IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.70f, editorArea);
		bottom.addView(TopLoopView.ID);
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
		bottom.addView("org.eclipse.pde.runtime.LogView");
	}

}

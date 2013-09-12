package edu.jhu.cs.bigbang.eclipse.wizard;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.wizards.newresource.BasicNewFileResourceWizard;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.ide.IDE;

import edu.jhu.cs.bigbang.eclipse.Activator;
/**
 * A wizard for creating a new Bigbang file.
 * @author Keeratipong Ukahcoke <kukacho1@jhu.edu>
 *
 */
public class NewFileWizard extends BasicNewFileResourceWizard {

	public static String ID = "edu.jhu.cs.bigbang.eclipse.wizard.NewFileWizard";
	
	private WizardNewFileCreationPage mainPage;

	@Override
	public void addPages() {
		this.mainPage = new WizardNewFileCreationPage(
				"BigBangNewFileWizardPage", this.getSelection());
		this.mainPage.setTitle("New BigBang File");
		this.mainPage.setDescription("Create a new BigBang File");
		this.addPage(this.mainPage);
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		this.setWindowTitle("New BigBang File");
		this.setNeedsProgressMonitor(true);
	}

	@Override
	protected void initializeDefaultPageImageDescriptor() {
		super.initializeDefaultPageImageDescriptor();
		String iconPath = "icons/";
		URL installURL = Activator.getDefault().getInstallURL();
		URL url;
		try {
			url = new URL(installURL, iconPath + "sample.gif");
			ImageDescriptor desc = ImageDescriptor.createFromURL(url);
			this.setDefaultPageImageDescriptor(desc);
		} catch (MalformedURLException e) {
			System.err.println("Error while getting file.");
			e.printStackTrace();
		}
	}

	@Override
	public boolean performFinish() {
		if (!this.mainPage.getFileName().toLowerCase().endsWith(".tb"))
			this.mainPage.setFileName(this.mainPage.getFileName() + ".tb");

		IFile file = this.mainPage.createNewFile();
		if (file == null)
			return false;
		this.selectAndReveal(file);
		IWorkbenchWindow dw = this.getWorkbench().getActiveWorkbenchWindow();
		try {
			if (dw != null) {
				IWorkbenchPage page = dw.getActivePage();
				if (page != null)
					IDE.openEditor(page, file, true);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return true;
	}
}

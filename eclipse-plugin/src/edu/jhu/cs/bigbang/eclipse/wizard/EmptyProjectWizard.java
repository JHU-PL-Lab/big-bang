package edu.jhu.cs.bigbang.eclipse.wizard;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

import edu.jhu.cs.bigbang.eclipse.Activator;




/**
 * A wizard for creating a new empty Bigbang Project
 * @author Keeratipong Ukahcoke <kukacho1@jhu.edu>
 *
 */
public class EmptyProjectWizard extends BasicNewProjectResourceWizard {

	@Override
	public void addPages() {
		super.addPages();
		super.setWindowTitle("New BigBang Project");
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
		super.performFinish();
		IProject project = super.getNewProject();
		IFolder srcFolder = project.getFolder("src");
		try {
			srcFolder.create(false, true, null);
		} catch (CoreException e) {
			System.err.print("Error while creating a src folder.");
			e.printStackTrace();
		}
		return true;
	}

}

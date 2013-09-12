package edu.jhu.cs.bigbang.eclipse.util;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;

import edu.jhu.cs.bigbang.eclipse.Activator;

public class ImageConstant {

	public static String CLEAR_ICON = "clear_icon.gif";

	public static ImageDescriptor getImageDescriptor(String name) {
		String iconPath = "icons/";
		try {
			URL installURL = Activator.getDefault().getInstalledURL();
			URL url = new URL(installURL, iconPath + name);
			return ImageDescriptor.createFromURL(url);
		} catch (MalformedURLException e) {
			System.err.println("Can't load image for '" + name + "'");
			return ImageDescriptor.getMissingImageDescriptor();
		}
	}

}

package system;

import java.io.File;

public class BIGZipFileFilter extends javax.swing.filechooser.FileFilter {
	public boolean accept(File f) {
		if (f.isDirectory())
		{
			return true;
		}
		if (f.isFile())
		{
			if( f.getName().endsWith(".zip") )
			{
				return true;
			}
		}
		return false;
	}
	
	public String getDescription()
	{
		return "*.ZIP";
	}

}

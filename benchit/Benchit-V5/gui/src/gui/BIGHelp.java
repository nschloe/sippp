/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGHelp.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: tschuet $
*  $Revision: 1.4 $
*  $Date: 2007/07/10 10:41:30 $
*
******************************************************************************/
package gui;


import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.net.URL;
import java.io.*;

import system.*;

import java.util.*;
import java.net.*;

public class BIGHelp extends JFrame
{
  private BIGInterface db = BIGInterface.getInstance();

  JEditorPane helpPane = new JEditorPane();
  JTree helpTree;
  DefaultListModel searchResultFieldModel = new DefaultListModel();
  JList searchResultField = new JList();
  JTextField searchStringField = new JTextField();
  boolean found = false;
  JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    private GridBagLayout gbLayout = new GridBagLayout();
    private GridBagConstraints gbConst = new GridBagConstraints();

  BIGStrings htmlFileList = new BIGStrings();
  ArrayList foundList;

/**
 * constructor, initialising components
 */
  public BIGHelp()
  {
  	initComponents();
    setTitle("BenchIT - Help");
    setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
    		+ File.separator + "gui" + File.separator + "img"
    		+ File.separator + "clock.png" ).getImage() );
    Object o=null;
   try
   {
      o = new URL( "http://www.benchit.org/wiki" ) ;
   }
   catch ( MalformedURLException ex )
   {
      System.err.println("malformed");
   }
    displayHelpText(o);
    setBounds(32,64,this.getWidth(),this.getHeight());
    pack();

  }


/**
 * Creates all major GUI components and arranges them.
 * Includes ListSelectionListener for JList, TreeSelectionListener for JTree and ChangeListener for JTabbedPane.
 */
  private void initComponents()
  {
    JPanel basePane = new JPanel();
//    JPanel BIGSRFPanel = new JPanel();


    DefaultMutableTreeNode top = new DefaultMutableTreeNode("BIG - BenchIT GUI");
    createNodes(top);

//-> spaeter durch suche im verzeichnis ersetzen
    htmlFileList.add("BIG_General.html");

    helpTree = new JTree(top);
    JScrollPane helpTreeViewScroll = new JScrollPane(helpTree,JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

    searchResultField.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    searchResultField.setLayoutOrientation(JList.VERTICAL);
    searchResultField.addListSelectionListener(new ListSelectionListener()
    {
    	public void valueChanged(ListSelectionEvent e)
    	{
            //System.err.println("BIGHelp: " + searchResultField.getSelectedIndex() + " : " + e.getValueIsAdjusting());
    		if ((found == true) && (searchResultField.getMaxSelectionIndex() >= 0)
                && (searchResultField.getSelectedIndex() >= 0)
                && (e.getValueIsAdjusting() == false))
    		{
                displayHelpText(foundList.get(searchResultField.getSelectedIndex()));
    		}
    	}
    });

    helpTree.addTreeSelectionListener(new TreeSelectionListener()
    {
      public void valueChanged(TreeSelectionEvent e)
      {
        setContentForTree();
      }
    });

    helpTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

    JPanel helpSearchPanel = new JPanel(gbLayout);

    JLabel exLabel = new JLabel("Please insert search string:");

    int ypos = 0;
    addComponent(exLabel, ypos++, 0, 1, 2, helpSearchPanel, GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTHWEST, 100, 0);

    searchStringField.setAlignmentX(Component.LEFT_ALIGNMENT);
  	searchStringField.setActionCommand("search");
  	searchStringField.addActionListener(new ActionListener()
    {
        public void actionPerformed(ActionEvent e) {
	        search(searchStringField.getText());
        }
    });
    addComponent(searchStringField, ypos, 0, 1, 1, helpSearchPanel, GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTHWEST, 100, 0);

    JButton searchButton = new JButton("Search");
  	searchButton.addActionListener(new ActionListener()
    {
        public void actionPerformed(ActionEvent e) {
	        search(searchStringField.getText());
        }
    });
  	searchButton.setActionCommand("search");

    addComponent(searchButton, ypos++, 1, 1, 1, helpSearchPanel, GridBagConstraints.NONE, GridBagConstraints.NORTHEAST, 0, 0);

    searchResultField.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

    JScrollPane searchResultFieldScroll = new JScrollPane(searchResultField);

    addComponent(searchResultFieldScroll, ypos++, 0, 1, 2, helpSearchPanel, GridBagConstraints.BOTH, GridBagConstraints.NORTHWEST, 100, 100);


    JTabbedPane helpTreeView = new JTabbedPane(JTabbedPane.TOP,JTabbedPane.WRAP_TAB_LAYOUT);
    helpTreeView.addTab("Index",helpTreeViewScroll);
    helpTreeView.addTab("Search",helpSearchPanel);
    helpTreeView.addChangeListener(new ChangeListener()
    {
    	public void stateChanged(ChangeEvent e)
    	{
    		int tabchoice = ((JTabbedPane)e.getSource()).getSelectedIndex();
			if(tabchoice == 1)
			{
    			helpPane.setText("");
    			searchResultField.setSelectedIndex(-1);
    			searchResultField.clearSelection();
    		}
			if(tabchoice == 0)
			{
    			if(helpTree.getSelectionCount() > 0) setContentForTree();
    			else helpPane.setText("");
    		}

    	}
    });

    helpPane.setEditable(false);
    helpPane.setContentType("text/html");
    helpPane.addHyperlinkListener(new HyperlinkListener()
    {
        public void hyperlinkUpdate(HyperlinkEvent e) {
            if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                displayHelpText(e.getURL());
            }
        }
    });
    JScrollPane helpContView = new JScrollPane(helpPane);

    splitPane.setTopComponent(helpTreeView);
    splitPane.setBottomComponent(helpContView);
    splitPane.setPreferredSize(new Dimension(700, 400));
    splitPane.setDividerLocation(240);

	JPanel buttonPanel = new JPanel();
	buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

	JButton closeHelpButton = new JButton("Close");
	closeHelpButton.addActionListener(new ActionListener()
    {
        public void actionPerformed(ActionEvent e) {
            setVisible(false);
        }
    });
  	//closeHelpButton.setActionCommand("close");

	buttonPanel.add(closeHelpButton);

	basePane.setLayout(new BoxLayout(basePane,BoxLayout.Y_AXIS));
  	buttonPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
  	splitPane.setAlignmentX(Component.LEFT_ALIGNMENT);

	basePane.add(splitPane);
	basePane.add(buttonPanel);

    getContentPane().add(basePane, BorderLayout.CENTER);
  }

/**
 * Creates the content accessed when using the JTree.
 */
  private void setContentForTree()
  {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)helpTree.getLastSelectedPathComponent();
        if (node == null) return;
        if (node.isLeaf())
        {
           // a big help with no h2s and h3s
           if ( ( ( ( String ) node.getUserObject() ).startsWith( "BIG" ) ) &&
                ( ( String ) node.getUserObject() ).endsWith( "html" ) )
               displayHelpText( createURL( ( ( String ) node.getUserObject() ) ) ) ;
           else
           {
               // is it under a big help?
               int depth=node.getLevel();
               DefaultMutableTreeNode tempNode=node;
               for (int i=depth;i>1;i--)
               {
                  tempNode=(DefaultMutableTreeNode)tempNode.getParent();
               }
               // setting the anchor
               String anchor=((String)(node.getUserObject()));
                   anchor=anchor.replaceAll(" ","_");
           // a big help with no h2s and h3s
           if ( ( ( ( String ) tempNode.getUserObject() ).startsWith( "BIG" ) ) &&
                ( ( String ) tempNode.getUserObject() ).endsWith( "html") )
               displayHelpText( createURL( ( ( String ) tempNode.getUserObject())+"#"+anchor ) ) ;
               else
               {
                   // the xml-entries
                  String filename = ( String ) ( ( DefaultMutableTreeNode )
                                                 helpTree.getSelectionPath().
                                                 getParentPath().
                                                 getLastPathComponent() ).
                      getUserObject() ;
                  helpPane.setText( db.getEntry( filename ,
                                                 ( String ) node.getUserObject() ).
                                    getHelp() ) ;

               }


           }

        } else
        {
           // a big help with h2s and h3s
           if ( ( ( ( String ) node.getUserObject() ).startsWith( "BIG" ) ) &&
                ( ( String ) node.getUserObject() ).endsWith( "html" ) )
              displayHelpText( createURL( ( ( String ) node.getUserObject() ) )) ;
           else
           {
              // is it under a big help?
              int depth = node.getLevel() ;
              DefaultMutableTreeNode tempNode = node ;
              for ( int i = depth ; i > 1 ; i-- )
              {
                 tempNode = ( DefaultMutableTreeNode ) tempNode.getParent() ;
              }
               // setting the anchor
               String anchor=((String)(node.getUserObject()));
                   anchor=anchor.replaceAll(" ","_");

              if ( ( ( ( String ) tempNode.getUserObject() ).startsWith( "BIG" ) ) &&
                   ( ( String ) tempNode.getUserObject() ).endsWith( "html" ) )
                 displayHelpText( createURL( ( ( String ) tempNode.getUserObject() )+"#"+anchor )) ;


           }


        }
  }

/**
 * This method constructs a valid URL from the given String value. Exceptions are caught and reported to StdErr.
 */
  private URL createURL(String file)
  {
      	URL baseURL = null;
              String prefix = "file://"+BIGInterface.getInstance().getBenchItPath() +
                  File.separator + "gui" + File.separator + "help" + File.separator ;
              if (file.indexOf(File.separator)!=-1)
                 prefix="file://";
              try
              {
                 baseURL = new URL( prefix + file ) ;
            }
            catch (java.net.MalformedURLException exc) {
                System.err.println("BIGHelp: not a valid url: " + baseURL);
                baseURL = null;
            }
            //System.err.println(baseURL);
         return baseURL;
  }

/**
 * This method sets the content of JEditorPane to the given URL. Exceptions are caught and reported to StdErr.
 */
  private void displayHelpText(Object what)
  {
    URL url = null;
    String text = null;
    try {
        url = (URL) what;
    }
    catch (Exception e) {
    }
    try {
        text = (String) what;
    }
    catch (Exception e) {
    }

    if (url != null) {
        try {
            helpPane.setPage(url);
        } catch (IOException e) {
    		System.err.println("BIGHelp: File not found: " + url);
     	}
    } else {
        if (text != null) {
            helpPane.setText("<html><body>\n" + text + "\n</body></html>");
        } else {
            System.err.println("BIGHelp: displayHelpText only understands url or string.");
        }
    }
    helpPane.repaint();
  }




/**
 * In this method an unlimited amount of HTML files given by their path in filenames are searched for a string located in searchString. All HTML tags are filtered before.
 * Returns the number of files, in which at least one hit occured.
 */
  private int searchHTML(String searchString,BIGStrings filenames)
  {

   try
   {
      URL u = new URL(
          "http://www.benchit.org/wiki/index.php/Special:Search?search=" +
          searchString + "&go=Go" ) ;
      this.displayHelpText( u ) ;
   }
   catch ( MalformedURLException ex )
   {
   }
    int count = 0;
    Iterator fileIt = filenames.iterator();

    while (fileIt.hasNext()) {
        BIGStrings htmlText = new BIGStrings();
        String filename = (String) fileIt.next();
        try {
            htmlText.readFromFile(filename);
        }
        catch (IOException ioe) {
            htmlText = new BIGStrings();
        }

        String pureText = htmlText.toString().replaceAll("\\<.*?\\>","").toLowerCase();
        if (pureText.indexOf(searchString.toLowerCase()) >= 0) {
            foundList.add(createURL(filename));
            StringBuffer aroundString=new StringBuffer("...");
            int index=pureText.indexOf(searchString.toLowerCase());
            int minIndex = Math.max(index-20,0);
            int maxIndex = Math.min(index+20,pureText.length());
            aroundString.append(pureText.substring(minIndex,maxIndex));
            aroundString.append("...");
            searchResultFieldModel.addElement(filename.substring(filename.lastIndexOf(File.separator)+1,filename.length())+"\t:\t"+aroundString);
            count++;
        }
    }

    return count;
  }

/**
 * Searches the database given by BIGInterface for occurrences of searchString. Returns the number of entry in which a hit occurred.
 */
  private int searchEntries(String searchString)
  {
    int count = 0;
    Iterator fileIt = db.getAllFilenames().iterator();

    while (fileIt.hasNext()) {
        String filename = (String) fileIt.next();
        Iterator entryIt = db.iterator(filename);
        while (entryIt.hasNext()) {
            BIGEntry entry = (BIGEntry) entryIt.next();
            String text = filename.replaceAll("<hostname>",db.getHost()) + " / " + entry.getViewName() + "<br>-----<br>" + entry.getHelp();
            if (text.toLowerCase().indexOf(searchString.toLowerCase()) >= 0) {
                foundList.add(text);
                //searchResultFieldModel.addElement(filename + File.separator + entry.getViewName());
                searchResultFieldModel.addElement(entry.getViewName());
                count++;
            }
        }

    }

  	return count;
  }

  public void search(String searchString) {
    displayHelpText("searching..");

    foundList = new ArrayList();
    htmlFileList = getHtmlFileList();
    searchResultFieldModel = new DefaultListModel();

    int count = searchHTML(searchString,htmlFileList)
                + searchEntries(searchString);
    if(count<=0) {
        found = false;
        displayHelpText("Sorry, no matches found");
    } else
    {
        found = true;
        displayHelpText("");
    }
    searchResultField.setModel(searchResultFieldModel);
  }

/**
 * Here all nodes for the JTree are created and linked below node top.
 */
  private void createNodes(DefaultMutableTreeNode top)
  {
  	ArrayList tempArray = new ArrayList();
          ArrayList tempArray2 = new ArrayList() ;
          BIGStrings files = this.getHtmlFileList() ;
          for ( int i = 0 ; i < files.size() ; i++ )
          {
             if (((files.get(i)).indexOf("BIG")>-1)&&(files.get(i)).endsWith("html"))
             {
                top.add(this.getNodesForFile(files.get(i)));

             }
          }
          //top.add(new DefaultMutableTreeNode("General Explanation"));
    //top.add(new DefaultMutableTreeNode("Index for more"));
    //top.add(new DefaultMutableTreeNode("Showing The Results"));

    DefaultMutableTreeNode paramsHelp = new DefaultMutableTreeNode("The Configuration Parameters");
    top.add(paramsHelp);

	tempArray = db.getAllFilenames().toArrayList();
    Iterator tempIterator = tempArray.iterator();
    while (tempIterator.hasNext())
    {
    	DefaultMutableTreeNode fileNode = new DefaultMutableTreeNode((String)(tempIterator.next()));
    	paramsHelp.add(fileNode);

		tempArray2 = db.getAllEntrynames((String)fileNode.getUserObject()).toArrayList();
        Iterator tempIterator2 = tempArray2.iterator();
		while (tempIterator2.hasNext())
        {
    	  DefaultMutableTreeNode entryNode = new DefaultMutableTreeNode((String)(tempIterator2.next()));
    	  fileNode.add(entryNode);
        }
    }
  }

    private void addComponent(Component comp, int pos, JPanel panel, int fill, int anchor, int wx, int wy) {
        addComponent(comp, pos, 0, 1, 1, panel, fill, anchor, wx, wy);
    }

    private void addComponent(Component comp, int ypos, int xpos, int height, int width, JPanel panel, int fill, int anchor, int wx, int wy) {
        gbConst.gridx = xpos;
        gbConst.gridy = ypos;
        gbConst.gridheight = height;
        gbConst.gridwidth = width;
        gbConst.fill = fill;
        gbConst.anchor = anchor;
        gbConst.insets = new Insets(1,3,1,3);
        gbConst.weightx = wx;
        gbConst.weighty = wy;
        gbLayout.setConstraints(comp, gbConst);
        panel.add(comp);
    }

    private BIGStrings getHtmlFileList() {
       BIGStrings result = new BIGStrings() ;

       File helpRoot = new File( BIGInterface.getInstance().getBenchItPath() +
                                 File.separator + "gui" + File.separator +
                                 "help") ;
        File[] subdirList = helpRoot.listFiles();
        try {
            for (int i = 0; i < subdirList.length; i++) {
                if ((subdirList[i].isFile()) && (subdirList[i].getName().endsWith(".html"))) {
                    result.add(helpRoot + File.separator + subdirList[i].getName());
                }
            }
		} catch (NullPointerException npe) {
			System.err.println("BIGHelp: getHtmlFileList: Maybe there are no files listet! " + npe);
		}
        return result;
    }
    private DefaultMutableTreeNode getNodesForFile(String file)
    {
       DefaultMutableTreeNode node = new DefaultMutableTreeNode( (new File(file)).getName() ) ;
       BIGStrings bs=new BIGStrings();
      try
      {
         bs.readFromFile( file ) ;
      }
      catch ( FileNotFoundException ex )
      {
      }
      catch ( IOException ex )
      {
      }
      String fileContent=bs.toString();
      {
         while (fileContent.indexOf("<h2>")>-1)
         {
            DefaultMutableTreeNode h2Node=new DefaultMutableTreeNode(fileContent.substring(fileContent.indexOf("<h2>")+4,fileContent.indexOf("</h2>")));
            fileContent=fileContent.substring(fileContent.indexOf("</h2>")+4,fileContent.length());
            node.add(h2Node);
            while((fileContent.indexOf("<h3>")>-1)&&(( fileContent.indexOf("<h3>")<fileContent.indexOf("<h2>"))||(fileContent.indexOf("<h2>")==-1)))
            {
               DefaultMutableTreeNode h3Node = new DefaultMutableTreeNode(
                   fileContent.substring( fileContent.indexOf( "<h3>" ) + 4 ,
                                          fileContent.indexOf( "</h3>" ) ) ) ;
               fileContent = fileContent.substring( fileContent.indexOf(
                   "</h3>" ) + 4 , fileContent.length() ) ;
               h2Node.add( h3Node ) ;
            }
         }
      }
       return node;
    }
}
/*****************************************************************************
Log-History

*****************************************************************************/

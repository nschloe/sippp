/*********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGKernelTree.java
 *
 *  Author: Robert Wloch
 *  Last change by: $Author: rschoene $
 *  $Revision: 1.8 $
 *  $Date: 2006/12/06 06:50:59 $
 *
 *********************************************************************/
package gui ;

import javax.swing.* ;
import javax.swing.tree.* ;
import javax.swing.event.* ;
import java.awt.* ;
import java.awt.event.* ;
import java.util.* ;
import java.io.* ;

import system.* ;

/**
 * The BIGKernelTree provides neccessary functionality for
 * the kernels an their tree view.
 **/
public class BIGKernelTree
    extends JTree
{
   private final static boolean debug = false ;
   /** Reference to the GUI. */
   private BIGGUI gui ;
   /** Reference to the tree's popup menu. */
   //private BIGPopupMenu popup ;
   /** Field reflecting the last sorting used in the tree. This value is
    * only valid until the next updateKernelTree() call. */
//   private int oldSorting = sorting;
   /** The TreeMap of the current sorting. */
   private TreeMap kernelTreeMap ;
   /** Sorting of the elements (which element of kernel is first)*/
   private int newSorting = 0 ;

   /**
    * The constructor.
    * @param newModel the TreeModel used in the tree.
    **/
   public BIGKernelTree( TreeModel newModel , BIGGUI gui )
   {
      super( newModel ) ;
      this.gui = gui ;

      this.addMouseListener( new MouseAdapter()
      {
         public void mouseClicked( MouseEvent evt )
         {
            if ( evt.getButton() != evt.BUTTON1 )
            {
               JPopupMenu popUp = computePopup( evt ) ;
               popUp.setInvoker( BIGKernelTree.this ) ;
               popUp.setRequestFocusEnabled( true ) ;
               popUp.setLocation( evt.getX() +
                                  ( int ) BIGKernelTree.this.getLocationOnScreen().
                                  getX() ,
                                  evt.getY() +
                                  ( int ) BIGKernelTree.this.getLocationOnScreen().
                                  getY() ) ;
               popUp.setVisible( true ) ;
               popUp.revalidate() ;
               // add(popUp);
            }
         }
      } ) ;

      try
      {
         newSorting = BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "lastKernelSortOrder" ) ;
      }
      catch ( Exception e )
      {
         newSorting = 0 ;
      }

      // items for the popup menu
      java.util.List itemList = new ArrayList() ;
      JMenuItem item ;

      // create popup menu
      // popup = new BIGPopupMenu( itemList, this, "KernelPopup" );
   }

   private JMenu getSortMenu()
   {
      // if (true)
      // {
      JMenu sortMenu = new JMenu( "Sort by" ) ;
      JMenuItem item = new JMenuItem( "type" ) ;
      ActionListener sortListener = new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            JMenuItem source = ( JMenuItem ) evt.getSource() ;
            //!!System.err.println(source.getText());
            for ( int i = 0 ; i < getSortMenu().getItemCount() ; i++ )
            {
               //!!System.err.println( ( ( JMenuItem ) getSortMenu().getItem( i ) ).
               //!!     getText()) ;
               if ( ( ( JMenuItem ) getSortMenu().getItem( i ) ).
                    getText().equals( source.getText() ) )
               {
                  //!!System.err.println("found");
                  //!!System.err.println("set sorting to "+i);
                  BIGKernelTree.this.setSorting( i ) ;
                  BIGKernelTree.this.updateKernelTree() ;
                  break ;

               }
            }
         }
      } ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      item = new JMenuItem( "name" ) ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      item = new JMenuItem( "language" ) ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      item = new JMenuItem( "parallelization libraries" ) ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      item = new JMenuItem( "other libraries" ) ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      item = new JMenuItem( "data type" ) ;
      item.addActionListener( sortListener ) ;
      sortMenu.add( item ) ;
      return sortMenu ;
      // }
   }

   private void buildTree(
       DefaultMutableTreeNode rootNode ,
       File directory )
   {
      if ( debug ) System.err.println(
          "-------------buildTree(KernelTree)-------------" ) ;
      Vector kernelVec = new Vector() ;
      // readKernels
      readKernels( kernelVec , directory ) ;
      // sort and insert kernels
      Vector sortedVec = new Vector() ;
      // get min
      while ( kernelVec.size() > 0 )
      {
         int position = 0 ;
         int min = Integer.MAX_VALUE ;
         String lastkernelString = null ;

         for ( int i = 0 ; i < kernelVec.size() ; i++ )
         {
            if ( lastkernelString == null )
            {
               position = i ;
               lastkernelString = ( ( BIGKernel ) kernelVec.get( i ) ).
                   getNameAfterSorting( this.newSorting ) ;
            }
            else

            if ( ( ( BIGKernel ) kernelVec.get( i ) ).getNameAfterSorting( this.
                newSorting ).compareTo( lastkernelString ) < 0 )
            {
               position = i ;
               lastkernelString = ( ( BIGKernel ) kernelVec.get( i ) ).
                   getNameAfterSorting( this.newSorting ) ;
            }
         }
         sortedVec.add( kernelVec.get( position ) ) ;
         kernelVec.removeElementAt( position ) ;
      }
      for ( int i = 0 ; i < sortedVec.size() ; i++ )
      {
         // insert Kernels
         addKernel( rootNode , ( BIGKernel ) sortedVec.get( i ) ) ;
      }

   }

   private void addKernel( DefaultMutableTreeNode rootNode ,
                           BIGKernel kernel )
   {
      if ( debug ) System.err.println( "New Sorting:" + this.newSorting ) ;
      kernel.setSorting( this.newSorting ) ;
      String name = kernel.getNameAfterSorting( this.newSorting ) ;
      if ( debug ) System.err.println( "Output-file:\n" + name ) ;
      DefaultMutableTreeNode tempNode = rootNode ;
      StringTokenizer stok = new StringTokenizer( name , "." ) ;
      String part ;
      boolean found ;
      // first the folders
      for ( int j = 0 ; j < kernel.getSortedNames( 0 ).length - 1 ; j++ )
      {
         part = stok.nextToken() ;
         found = false ;

         for ( int i = 0 ; i < tempNode.getChildCount() ; i++ )
         {
            if ( ( ( DefaultMutableTreeNode ) tempNode.getChildAt( i ) ).
                 toString().equals( part ) )
            {
               tempNode = ( DefaultMutableTreeNode ) tempNode.getChildAt( i ) ;
               found = true ;
               break ;
            }
         }
         if ( !found )
         {
            DefaultMutableTreeNode newNode = new DefaultMutableTreeNode( part ) ;
            if ( debug ) System.err.println( "Adding " + newNode + " to " +
                                             tempNode ) ;
            tempNode.add( newNode ) ;
            tempNode = newNode ;
         }
      }
      // then the filename
      part = stok.nextToken() ;
      found = false ;

      for ( int i = 0 ; i < tempNode.getChildCount() ; i++ )
      {
         if ( ( ( DefaultMutableTreeNode ) tempNode.getChildAt( i ) ).
              toString().equals( part ) )
         {
            tempNode = ( DefaultMutableTreeNode ) tempNode.getChildAt( i ) ;
            found = true ;
            break ;
         }
      }
      if ( !found )
      {
         DefaultMutableTreeNode newNode = new DefaultMutableTreeNode( kernel ) ;
         if ( debug ) System.err.println( "Adding " + newNode + " to " +
                                          tempNode ) ;
         tempNode.add( newNode ) ;
         tempNode = newNode ;
      }

   }

   public void readKernels( java.util.Vector v , File directory )
   {
      File[] subFiles = directory.listFiles( new FileFilter()
      {
         public boolean accept( File f )
         {
            if ( f.isDirectory() )
               if ( !f.getName().equals( "CVS" ) )
                  return true ;
            return false ;
         }
      } ) ;
      if ( subFiles == null )return ;
      for ( int i = 0 ; i < subFiles.length ; i++ )
      {
         if ( ! ( new File( subFiles[ i ].getAbsolutePath() + File.separator +
                            "COMPILE.SH" ) ).
              exists() )
         {
            this.readKernels( v , subFiles[ i ] ) ;
         }
         else
         {
            try
            {
               v.add( new BIGKernel( subFiles[ i ] , null ) ) ;
            }
            catch ( StringIndexOutOfBoundsException ex )
            {
               System.err.println(subFiles[ i ]+" is not a valid kernel");
            }
            if ( debug ) System.err.println( "Found new kernel " + subFiles[ i ] ) ;
         }
      }

   }

   /**
    * Sets the sorting field of the tree to the given value.
    * For valid values look at the documentation of BIGExecute. An invalid
    * sorting flag will default to BIGExecute.KERNEL_DIRECTORY.
    * If the new sorting is different than the current one the kernel tree
    * will be updated, otherwise nothing will happen.
    *
    * @param newSorting The new sort order for the kernel tree.
    **/
   private void setSorting( int newSorting )
   { /*
           if ( sorting == newSorting ) return;
           sorting = newSorting;
           BIGInterface.getInstance().getBIGConfigFileParser().set(
                   "lastKernelSortOrder", Integer.toString( sorting ) );*/
      this.newSorting = newSorting ;

       BIGInterface.getInstance().getBIGConfigFileParser().set(
           "lastKernelSortOrder" , "" + newSorting ) ;

       /* Thread t = new Thread() {
            public void run() {
                BIGKernelTree.this.updateKernelTree();
                BIGKernelTree.this.gui.getResultTree().kernelSortingChanged();
            }
        };
        SwingUtilities.invokeLater( t );*/
    }

   /**
    * Initital setup of the kernel tree. Should be called only once.
    * Use updateKernelTree for later updates and reloads.
    **/
   public void setupTree()
   {
      setRootVisible( true ) ;
      setEditable( false ) ;
      setShowsRootHandles( true ) ;
      setCellRenderer( new KernelTreeCellRenderer( gui ) ) ;
      setExpandsSelectedPaths( true ) ;
      DefaultTreeModel model = ( DefaultTreeModel ) getModel() ;
      DefaultMutableTreeNode rootNode =
          ( DefaultMutableTreeNode ) ( model.getRoot() ) ; /*
                  BIGExecute exec = BIGExecute.getInstance();
                  Map kernelList = exec.getKernelList( sorting );
                  kernelTreeMap = new TreeMap( kernelList );
                  TreeSet keys = new TreeSet( kernelTreeMap.keySet() );
                  Iterator iter = keys.iterator();
                  addKernelsToTree( model, rootNode, kernelTreeMap, iter );*/
     this.buildTree( rootNode ,
                     new File( BIGInterface.getInstance().getBenchItPath() +
                               File.separator + "kernel" + File.separator ) ) ;
      updateUI() ;
   }

   /**
    * This is a special update telling BIGExecute to check for new source
    * languages first. This update will also be done only, if the kernel
    * tree is in SOURCE_LANGUAGE sorting.
    * @deprecated
    **/
   public void updateKernelTreeCompletely()
   {
      updateKernelTree() ;
   }

   /**
    * Updates the kernel tree.
    **/
   public void updateKernelTree()
   {
      Thread t = new Thread()
      {
         public void run()
         {
            // TreePath[] oldSelection = getSelectionPaths();
            DefaultTreeModel model = ( DefaultTreeModel ) getModel() ;
            DefaultMutableTreeNode rootNode =
                ( DefaultMutableTreeNode ) ( model.getRoot() ) ;
            if ( debug ) System.err.println( "Childs:" + rootNode.getChildCount() ) ;
            rootNode.removeAllChildren() ;
            if ( debug ) System.err.println( "Childs:" + rootNode.getChildCount() ) ;
                /*
                              BIGExecute exec = BIGExecute.getInstance();

                              exec.reloadKernelList();
                              Map kernelList = exec.getKernelList( sorting );

                              kernelTreeMap = new TreeMap( kernelList );
            TreeSet keys = new TreeSet( kernelTreeMap.keySet() );
                              Iterator iter = keys.iterator();
            addKernelsToTree( model, rootNode, kernelTreeMap, iter );*/

           BIGKernelTree.this.buildTree( rootNode ,
                                         new File( BIGInterface.getInstance().
               getBenchItPath() +
               File.separator + "kernel" + File.separator ) ) ;
            model.setRoot( rootNode ) ;

            /*      if ( oldSorting != sorting ) {
                     oldSorting = sorting;
                  }
                  else {
                     setSelectionPaths( oldSelection );
                  }*/

         }
      } ;
      SwingUtilities.invokeLater( t ) ;

   }

   /**
    * Recursive method to add all kernel and directory names
    * to the kernel tree.
    *
    * @param model The model of the tree.
    * @param parentNode The parent node for new nodes.
    * @param parent The map representing the tree structure.
    * @param it The iterator over the map's keys.
    **/
   /*   private void addKernelsToTree( DefaultTreeModel model,
         DefaultMutableTreeNode parentNode, Map parent, Iterator it ) {
         while ( it.hasNext() ) {
            DefaultMutableTreeNode childNode = null;
            String key = (String)it.next();
            if ( key.equals( "<kernels>" ) ) {
               // add kernels to the tree
               TreeSet childSet = (TreeSet)(parent.get( key ));
               Iterator it2 = childSet.iterator();
               while ( it2.hasNext() ) {
                  Object nodeObj = it2.next();
                  if ( nodeObj instanceof BIGKernel ) {
                     // it's a kernel
                     BIGKernel kernel = (BIGKernel)nodeObj;
                     gui.getKernels().add( kernel.getRelativePath() );
                     childNode = new DefaultMutableTreeNode( kernel );
                     model.insertNodeInto(
                        childNode, parentNode, parentNode.getChildCount() );
                  }
                  else {
                     // it's a subdir in the subdir
                     key = nodeObj.toString();
                     // add subdirname and recurse subdirectory
                     childNode = new DefaultMutableTreeNode( key );
                     model.insertNodeInto(
                           childNode, parentNode, parentNode.getChildCount() );
                     Map child = (Map)parent.get( key );
                     Iterator it3 = (new TreeSet( child.keySet() )).iterator();
                     addKernelsToTree( model, childNode, child, it3 );
                  }
               }
            }
            else {
               // add subdirname and recurse subdirectory
               childNode = new DefaultMutableTreeNode( key );
               model.insertNodeInto(
                     childNode, parentNode, parentNode.getChildCount() );
               Map child = (Map)parent.get( key );
               Iterator it2 = (new TreeSet( child.keySet() )).iterator();
               addKernelsToTree( model, childNode, child, it2 );
            }
         }
      }*/

   private int getNumberOfSubKernels( DefaultMutableTreeNode node )
   {
      int files = 0 ;
      for ( int i = 0 ; i < node.getChildCount() ; i++ )
      {
         if ( node.getChildAt( i ).isLeaf() )
            files++ ;
         else
            files = files +
                getNumberOfSubKernels( ( DefaultMutableTreeNode ) node.
                                       getChildAt( i ) ) ;

      }
      return files ;
   }

   /*private void setSelectedTreePath(TreePath[] path)
   {

      TreeSelectionListener[] listeners = getTreeSelectionListeners() ;
      for ( int i = 0 ; i < listeners.length ; i++ )
         removeTreeSelectionListener( listeners[ i ] ) ;
      for ( int i = 0 ; i < path.length ; i++ )
      {
         DefaultMutableTreeNode dmtn = ( DefaultMutableTreeNode ) path[
             i ].
             getLastPathComponent() ;
         TreePath[] subpaths= new TreePath[dmtn.getChildCount()];
         for ( int j = 0 ; j < dmtn.getChildCount() ; j++ )
            subpaths[j]=path[ i ].pathByAddingChild(
                dmtn.getChildAt( j ) );
            setSelectedTreePath( subpaths ) ;
      }
      this.addSelectionPaths(path);
      for ( int i = 0 ; i < listeners.length ; i++ )
         addTreeSelectionListener( listeners[ i ] ) ;

   }

   private void removeSelectedTreePath(TreePath[] path)
   {
      TreeSelectionListener[] listeners = getTreeSelectionListeners() ;
      for ( int i = 0 ; i < listeners.length ; i++ )
         removeTreeSelectionListener( listeners[ i ] ) ;

      for (int j=0;j<path.length;j++)
      {
         DefaultMutableTreeNode dmtn = ( DefaultMutableTreeNode ) path[j].
             getLastPathComponent() ;
         if ( !dmtn.isRoot() )
            subRemoveSelectedTreePath( path[j].getParentPath() ) ;
         TreePath[] paths=new TreePath[dmtn.getChildCount()];
         for ( int i = 0 ; i < dmtn.getChildCount() ; i++ )
         {
            paths[i]=path[j].pathByAddingChild(
                dmtn.getChildAt( i ) );
         }
            removeSelectedTreePath( paths  ) ;

      }
      for ( int i = 0 ; i < listeners.length ; i++ )
      {
         addTreeSelectionListener( listeners[ i ] ) ;
         listeners[i].valueChanged(null);
      }

   }
   private void subRemoveSelectedTreePath(TreePath path)
   {
      TreeSelectionListener[] listeners = getTreeSelectionListeners() ;
      for ( int i = 0 ; i < listeners.length ; i++ )
         removeTreeSelectionListener( listeners[ i ] ) ;

      DefaultMutableTreeNode dmtn = ( DefaultMutableTreeNode ) path.
          getLastPathComponent() ;
      if ( !dmtn.isRoot() )
         subRemoveSelectedTreePath( path.getParentPath() ) ;
      for ( int i = 0 ; i < listeners.length ; i++ )
      {
         addTreeSelectionListener( listeners[ i ] ) ;
         listeners[i].valueChanged(null);
      }

   }*/




   /**
    * Sets up a TreeSelectionListener and a MouseListener and adds them
    * to the tree.
    **/
   public void setListeners()
   {
      this.addTreeExpansionListener( new TreeExpansionListener()
      {
         private void expandAll( JTree tree , TreePath parent , boolean expand )
         {
            // Traverse children
            TreeNode node = ( TreeNode ) parent.getLastPathComponent() ;
            if ( debug ) System.err.println( "expandAll:" + expand + "/" + node ) ;
            if ( node.getChildCount() >= 0 )
            {
               for ( Enumeration e = node.children() ; e.hasMoreElements() ; )
               {
                  TreeNode n = ( TreeNode ) e.nextElement() ;
                  TreePath path = parent.pathByAddingChild( n ) ;
                  expandAll( tree , path , expand ) ;
               }
            }

            // Expansion or collapse must be done bottom-up
            if ( expand )
            {
               tree.expandPath( parent ) ;
            }
            else
            {
               tree.collapsePath( parent ) ;
            }
         }

         public void treeExpanded( TreeExpansionEvent tEvt )
         {
            TreePath path = tEvt.getPath() ;
            DefaultMutableTreeNode lastNode = ( DefaultMutableTreeNode )
                path.getLastPathComponent() ;
            if ( lastNode.getUserObject().equals( "all kernels" ) )
               return ;
            if ( debug ) System.err.println( path ) ;

            if ( getNumberOfSubKernels( lastNode ) < 8 )
            {
               expandAll( BIGKernelTree.this , path , true ) ;
            }
         }

         public void treeCollapsed( TreeExpansionEvent tEvt )
         {

         }

      } ) ;

      // adds tree selection listener for view display change
      addTreeSelectionListener( new TreeSelectionListener()
      {
         Thread thread = null ;
         public void valueChanged( TreeSelectionEvent tse )
         { // called on selection change only
            if ( gui.getState() != gui.KERNELS )
            {
               gui.setGUIState( gui.KERNELS ) ;
            }

            final TreePath path[] = getSelectionPaths() ;
            // add children
            // maybe implemented later (add shold work, but remove?)
            if (tse.isAddedPath())
            {
               //setSelectedTreePath(path);
            }
            else
            {
               //removeSelectedTreePath(path);
            }
            /*else
            {
               // I dont THINK this is still in use

               // check if there is at least one element
               // selected in the kernel tree
               TreePath[] paths = getSelectionPaths() ;
               if ( ( paths != null ) && ( paths.length > 0 ) )
               {
                  // at least on selection exists
                  for ( int i = 0 ; i < paths.length ; i++ )
                  {
                     // iterate over the selected elements
                     // search for selected kernel files first
                     path = paths[ i ] ;
                     Object obj = path.getLastPathComponent() ;
                     String name = obj.toString() ;
                     if ( gui.getKernels().contains( name ) )
                     {
                        // we found a selected kernel file
                        break ;
                     }
                     else path = null ;
                  }
                  // no kernel file is selected
                  // but if there are kernel classes selected
                  // get the first kernel file out of them
                  if ( path == null )
                  {
                     // no kernel file is selected
                     // but if there are kernel classes selected
                     // set the path of the first kernel class
                     path = paths[ 0 ] ;
                  }
               } // end of if ( paths.length > 0 )
            } // end of else of if ( e.isAddedPath() )
            */
            if ( path != null )
            {
               if ( thread != null && thread.isAlive() )
               {
                  // this code is unsafe, because monitors may remain locked
                  System.out.println(
                      "Still loading other kernel. Please wait." ) ;
                  // could use it, does work, but is deprecated :(
                  //thread.stop();
                  return ;
               }
               if ( gui.getKernelScriptWindow() != null )
               {
                  // save changed files
                  if ( gui.getKernelScriptWindow() != null )
                     gui.getKernelScriptWindow().saveRequest( true ) ;

                  gui.getKernelScriptWindow().removeAll() ;
                  gui.setKernelScriptWindow( null ) ;
               }
               //final TreePath pth = path ;
               thread = new Thread()
               {
                  public void run()
                  {
                     try
                     {
                        new BIGKernelScriptWindow(
                            path ,
                            BIGInterface.getInstance().getBIGConfigFileParser()
                            .boolCheckOut( "kernelSourceEdit" ) , gui ) ;
                     }
                     catch ( Exception exc )
                     {
                           new BIGKernelScriptWindow( path , gui ) ;
                     }
                     if (interrupted())
                     {
                        System.err.println("int");
                     }
                  }
               } ;
               thread.start() ;
            }
         } // end of valueChanged()
      } ) ; // end of addTreeSelectionListener()

      // adds mouse listener for pop up menu
      addMouseListener( new MouseAdapter()
      {
         public void mousePressed( MouseEvent e )
         {
            if ( gui.getState() != gui.KERNELS )
            {
               gui.setGUIState( gui.KERNELS ) ;
            }
            int selRow = getRowForLocation(
                e.getX() , e.getY() ) ;
            TreePath selPath = getPathForLocation(
                e.getX() , e.getY() ) ;

            if ( ( e.getClickCount() == 2 ) && !e.isPopupTrigger() )
            { // double left click in LINUX
               if ( selPath == null ) // double clicking symbol before name
                  return ;
               if ( gui.getKernels().contains(
                   selPath.getLastPathComponent().toString() ) )
               {
                  gui.executeSelected() ;
               }
            }
         }
      } ) ;
      // JTree MUST be registered manually to display tool tips
      ToolTipManager.sharedInstance().registerComponent( this ) ;
   }

   /**
    * Recursive method to add all kernel names under a node to
    * a BIGStrings object.
    *
    * @param result The BIGStrings object to fill.
    * @param parentNode The "root node" of the sub tree to search in.
    **/
   public static void addKernelsToBIGString(
       BIGStrings result , DefaultMutableTreeNode parentNode )
   {
      int end = parentNode.getChildCount() ;
      // traverse all children of parentNode to add all kernels
      for ( int i = 0 ; i < end ; i++ )
      {
         DefaultMutableTreeNode childNode =
             ( DefaultMutableTreeNode ) parentNode.getChildAt( i ) ;
         // get the node's name
         Object nodeObj = childNode.getUserObject() ;
         String nodeName = nodeObj.toString() ;
         // if nodeObj is instance of BIGKernel add that kernel name
         if ( nodeObj instanceof BIGKernel )
         {
            // System.err.println("Adding "+((BIGKernel)nodeObj).getRelativePath());
            result.add( ( ( BIGKernel ) nodeObj ).getAbsolutePath() ) ;
         }
         else
         {
            // it's another subdir name -> recurse
            // the root node falls also into this branch
            addKernelsToBIGString( result , childNode ) ;
         }
      }
   }

   /**
    * Searches the model of the tree for the TreeNode specified
    * by the TreePath.
    *
    * @param selectedPath The TreePath to find the node for.
    * @return the DefaultMutableTreeNode or null, if node not found.
    **/
   public DefaultMutableTreeNode findNode( TreePath selectedPath )
   {

      DefaultMutableTreeNode result = null ;

      String kernelName = selectedPath.getLastPathComponent().toString() ;
      DefaultTreeModel model = ( DefaultTreeModel ) getModel() ;
      DefaultMutableTreeNode rootNode = ( DefaultMutableTreeNode ) model.
          getRoot() ;
      // find tree node of selected node in the tree
      boolean nodeFound = false ;
      DefaultMutableTreeNode parent = rootNode ;
      for ( int k = 1 ; k < selectedPath.getPathCount() ; k++ )
      {
         Object pathObj = selectedPath.getPathComponent( k ) ;
         if ( pathObj == null )break ;
         int end = parent.getChildCount() ;
         boolean childFound = false ;
         for ( int l = 0 ; l < end ; l++ )
         {
            DefaultMutableTreeNode childNode =
                ( DefaultMutableTreeNode ) parent.getChildAt( l ) ;
            if ( pathObj.toString().compareTo( childNode.toString() ) == 0 )
            {
               childFound = true ;
               parent = childNode ;
               if ( kernelName.compareTo( childNode.toString() ) == 0 )
               {
                  result = childNode ;
                  nodeFound = true ;
               }
               break ;
            }
         }
         if ( !childFound || nodeFound )break ;
      }
      return result ;
   }

   /**
    * Returns the TreeMap used for the current sorting. this Map must not
    * be changed but only used for analysing the tree structure.
    *
    * @return the TreeMap representing the tree structure.
    **/
   public TreeMap getCurrentTreeMap()
   {
      return kernelTreeMap ;
   }

   public BIGKernel findKernel( String s )
   {
      BIGKernel retKernel = null ;
      DefaultMutableTreeNode rootNode = ( DefaultMutableTreeNode ) getModel().
          getRoot() ;
      Stack nodeStack = new Stack() ;
      nodeStack.add( rootNode ) ;
      while ( !nodeStack.isEmpty() )
      {
         DefaultMutableTreeNode dmtn = ( DefaultMutableTreeNode ) nodeStack.pop() ;
         if ( dmtn.getUserObject() instanceof BIGKernel )
         {
            if ( ( ( BIGKernel ) dmtn.getUserObject() ).getAbsolutePath().
                 equals( s ) )
            {
               retKernel = ( BIGKernel ) dmtn.getUserObject() ;
               return retKernel ;
            }

         }
         else
         {
            for ( int i = 0 ; i < dmtn.getChildCount() ; i++ )
            {
               nodeStack.push( dmtn.getChildAt( i ) ) ;
            }
         }

      }
      return retKernel ;
   }

   public void reloadKernelsExecutables()
   {
      // first find all kernels
      DefaultMutableTreeNode rootNode = ( DefaultMutableTreeNode ) getModel().
          getRoot() ;
      Stack kernelStack = new Stack() ;
      Stack nodeStack = new Stack() ;
      nodeStack.push( rootNode ) ;
      while ( !nodeStack.isEmpty() )
      {
         DefaultMutableTreeNode dmtn = ( DefaultMutableTreeNode ) nodeStack.pop() ;
         if ( dmtn.getUserObject() instanceof BIGKernel )
         {
            kernelStack.push( dmtn.getUserObject() ) ;

         }
         else
         {
            for ( int i = 0 ; i < dmtn.getChildCount() ; i++ )
            {
               nodeStack.push( dmtn.getChildAt( i ) ) ;
            }
         }
      }
      // second reload all Kernels executables
      while ( !kernelStack.isEmpty() )
      {
         BIGKernel kernel = ( BIGKernel ) kernelStack.pop() ;
//         System.err.println(kernel);
         kernel.reloadExecutables() ;
      }

   }

   private JPopupMenu computePopup( MouseEvent evt )
   {
      JPopupMenu pop = new JPopupMenu() ;
      final TreePath path = this.getClosestPathForLocation( evt.
          getX() , evt.getY() ) ;
      DefaultMutableTreeNode selectedNode = ( ( DefaultMutableTreeNode ) path.
                                              getLastPathComponent() ) ;
      JMenuItem sortMenu = getSortMenu() ;
      JMenuItem item ;
      item = new JMenuItem( "Execute local" ) ;
      item.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            BIGKernelTree.this.gui.executeSelected() ;
         }
      } ) ;
      pop.add( item ) ;

      item = new JMenuItem( "Execute in remote folder" ) ;
      item.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            BIGKernelTree.this.gui.executeSelectedOnOtherSystem() ;
         }
      } ) ;
      pop.add( item ) ;
// set a flag for which radio button is active at first
      /*      boolean sourceSorting = false;
       if ( sorting == BIGExecute.getInstance().SOURCE_LANGUAGE ) {
          sourceSorting = true;
       }
// create the radio button button group
       ButtonGroup group = new ButtonGroup();*/
      pop.addSeparator() ;
      pop.add( sortMenu ) ;
      // if it is a node (a kernel)
      if (selectedNode.getUserObject() instanceof system.BIGKernel)
      {
         final BIGKernel kernel=(BIGKernel)selectedNode.getUserObject();
         item = new JMenuItem( "Duplicate" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {

               cloneKernel(kernel);
            }
         } ) ;

         pop.addSeparator() ;
         pop.add(item);
         item = new JMenuItem("Remove");
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               if ( JOptionPane.showConfirmDialog( BIGKernelTree.this ,
                   "Do you really want to delete the kernel \""+kernel.getNameAfterSorting(0)+"\"?" ,
                   "Deleting kernel" ,
                   JOptionPane.YES_NO_OPTION ) ==
                    JOptionPane.OK_OPTION )
               {
                  BIGFileHelper.remove( new File( kernel.getAbsolutePath() ) ) ;
                  BIGKernelTree.this.updateKernelTree();
               }
            }
         } ) ;
         pop.add(item);

         pop.addSeparator() ;
         item = new JMenuItem("create new File");
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               String name = JOptionPane.showInputDialog( BIGKernelTree.this ,
                   "Please type the name of the new file." ,
                   "Creating new File for kernel " +
                   kernel.getNameAfterSorting( 0 ) ,
                        JOptionPane.OK_CANCEL_OPTION ) ;
               if ( ( name == null ) || ( name == "" ) )
               {
                  System.err.println( "Won't create File without name" ) ;
                  return ;
               }
               File f = new File( kernel.getAbsolutePath() + File.separator +
                                  name ) ;
               if ( f.exists() )
               {
                  System.err.println( "File " + name + " already exists" ) ;
                  return ;
               }
               try
               {
                  f.createNewFile() ;
               }
               catch ( IOException ex )
               {
                  System.err.println( "Couldn't create file " + name ) ;
               }

               TreeSelectionListener lis = getTreeSelectionListeners()[0] ;
               TreePath[] paths=new TreePath[1];
               paths[0]=path;
               boolean[] isNew=new boolean[1];
               isNew[0]=false;
               lis.valueChanged(new TreeSelectionEvent(BIGKernelTree.this,paths,isNew,null,null));

            }
         } ) ;
         pop.add(item);
         item = new JMenuItem("remove single File");
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               String[] options=new String[kernel.getFiles().length];
               for (int i=0;i<options.length;i++)
               {
                  options[i]=kernel.getFiles()[i].getName();
               }
               int retVal=JOptionPane.showOptionDialog(BIGKernelTree.this,"select the file you'd like to remove","Remove single file",JOptionPane.OK_CANCEL_OPTION,0,null,options,options[0]);
               if ( retVal>-1)
               {
                  File f = kernel.getFiles()[retVal];
                  f.delete();
               }
               TreeSelectionListener lis = getTreeSelectionListeners()[0] ;
               TreePath[] paths=new TreePath[1];
               paths[0]=path;
               boolean[] isNew=new boolean[1];
               isNew[0]=false;

               lis.valueChanged(new TreeSelectionEvent(BIGKernelTree.this,paths,isNew,null,null));

            }
         } ) ;
         pop.add(item);



      }
      return pop ;
   }

   private void cloneKernel(BIGKernel kernel)
   {
      JPanel main = new JPanel(new GridLayout(2,5)) ;
      JTextField genre = new JTextField( kernel.getType() ,
                                         kernel.getType().length()) ;
      JTextField algorithm = new JTextField( kernel.getName() ,
                                             kernel.getName().length()) ;
      JTextField language = new JTextField( kernel.getSourceLanguage() ,
                                            kernel.getSourceLanguage().length()) ;
      JTextField parLib = new JTextField( kernel.getParallelLibraries() ,
                                          kernel.getParallelLibraries().length() ) ;
      JTextField lib = new JTextField( kernel.getLibraries() ,
                                       kernel.getLibraries().length() ) ;
      JTextField datatype = new JTextField( kernel.getDataType() ,
                                            kernel.getDataType().length() ) ;
      main.add(new JLabel(" Genre "));
      main.add(new JLabel(" Algorithm "));
      main.add(new JLabel(" Language "));
      main.add(new JLabel(" parallel Libraries "));
      main.add(new JLabel(" other Libraries "));
      main.add(new JLabel(" Type/Datatype "));
      main.add( genre ) ;
      main.add(algorithm);
      main.add(language);
      main.add(parLib);
      main.add(lib);
      main.add(datatype);
      int returnValue=JOptionPane.showConfirmDialog(this,main,"Insert the new name of the kernel",JOptionPane.OK_CANCEL_OPTION);
      if (returnValue==JOptionPane.OK_OPTION)
      {
         String kernelpath = BIGInterface.getInstance().getBenchItPath() +
             File.separator + "kernel" + File.separator ;
         File newKernelDir = new File( kernelpath + genre.getText() +
                                       File.separator + algorithm.getText() +
                                       File.separator + language.getText() +
                                       File.separator + parLib.getText() +
                                       File.separator + lib.getText() + File.separator +
                                       datatype.getText() ) ;
         if (newKernelDir.exists() )
         {
            System.out.println("This kernel already exists!");
            return;
         }
         newKernelDir.mkdirs();
         while (!newKernelDir.exists());
         // copy
         File[] oldFiles = kernel.getFiles() ;
         for ( int i = 0 ; i < oldFiles.length ; i++ )
         {
            BIGFileHelper.copyToFolder( oldFiles[ i ] , newKernelDir , true ) ;
         }
         DefaultTreeModel model = ( DefaultTreeModel ) getModel() ;
         DefaultMutableTreeNode rootNode =
             ( DefaultMutableTreeNode ) ( model.getRoot() ) ;
         BIGKernel newKernel = null ;
         try
         {
            newKernel = new BIGKernel( newKernelDir , null ) ;
         }
         catch ( StringIndexOutOfBoundsException ex )
         {
            System.err.println(newKernelDir +" is not a valid kernel");
         }
         this.addKernel( rootNode , newKernel ) ;
         BIGKernelTree.this.updateKernelTree() ;
      }
      }
   /**
    * The kernel tree's cell renderer. This class chooses the
    * appropriate icons for tree nodes and displays tool tip
    * texts if the mouse cursor is above a tree node.
    **/
   class KernelTreeCellRenderer
       extends DefaultTreeCellRenderer
   {
      ImageIcon kernelIcon ;
      ImageIcon kernelBaseOpenedIcon ;
      ImageIcon kernelBaseClosedIcon ;
      BIGGUI big ;
      BIGInterface bigInterface ;
      /**
       * Constructs a new tree cell renderer.
       **/
      public KernelTreeCellRenderer( BIGGUI gui )
      {
         big = gui ;
         bigInterface = BIGInterface.getInstance() ;
         String imgPath = bigInterface.getBenchItPath()
             + File.separator + "gui" + File.separator + "img"
             + File.separator ;
         kernelIcon = new ImageIcon( imgPath + "kernel16.png" ) ;
         kernelBaseOpenedIcon = new ImageIcon(
             imgPath + "kernel_opened16.png" ) ;
         kernelBaseClosedIcon = new ImageIcon(
             imgPath + "kernel_closed16.png" ) ;
      }

      /**
       * Overriding super class' method.
       **/
      public java.awt.Component getTreeCellRendererComponent( JTree tree ,
          Object value , boolean sel , boolean expanded , boolean leaf ,
          int row ,
          boolean hasFocus )
      {
         super.getTreeCellRendererComponent(
             tree , value , sel , expanded , leaf , row , hasFocus ) ;
         DefaultMutableTreeNode node = ( DefaultMutableTreeNode ) value ;
         setIcon( kernelIcon ) ; // default node icon
         if ( node.equals( node.getRoot() ) )
         {
         /* set icon for root node */
            setIcon( kernelBaseOpenedIcon ) ;
         }
         else if ( !node.isLeaf() )
         {
         /* set icons for parent nodes */
            if ( expanded )
               setIcon( kernelBaseOpenedIcon ) ;
            else
               setIcon( kernelBaseClosedIcon ) ;
         }
         Object o = node.getUserObject() ;
         if ( o instanceof BIGKernel )
         {
            // a kernel tool tip is needed
            String tip = ( ( BIGKernel ) o ).getRelativePath() ;
            this.setToolTipText( tip ) ;
         }
         else
         {
            // a subdir tool tip is needed
            int count = BIGKernelTree.this.getNumberOfSubKernels( node ) ;
            String tip = "The subdirectory  \"" + o.toString() + "\" " ;
            if ( node.equals( node.getRoot() ) )
            {
               tip = "The kernel directory " ;
            }
            tip = tip + "contains " + count + " kernel" ;
            if ( count > 1 ) tip = tip + "s" ;
            tip = tip + "." ;
            this.setToolTipText( tip ) ;
         }
         return this ;
      }
   }
}
/******************************************************************************
 *  Log-History
 *
 *  $Log: BIGKernelTree.java,v $
 *  Revision 1.8  2006/12/06 06:50:59  rschoene
 *  surrounded new BIGKernel with try-catch
 *
 *  Revision 1.7  2006/09/28 04:09:37  rschoene
 *  commenting
 *
 *  Revision 1.6  2006/03/06 09:28:54  rschoene
 *  added some single file options
 *
 *  Revision 1.5  2005/12/15 11:27:02  rschoene
 *  changes in duplicate kernel
 *
 *  Revision 1.4  2005/12/15 11:11:09  rschoene
 *  duplicate-dialog has larger textfields
 *
 *  Revision 1.3  2005/12/14 16:01:18  rschoene
 *  added duplicate und delete
 *
 *  Revision 1.2  2005/11/11 13:43:45  rschoene
 *  now helps KernelScriptWindow showing all selected kernels
 *
 *  Revision 1.1  2005/10/20 13:13:56  rschoene
 *  src3 add
 *
 *  Revision 1.17  2005/06/16 10:46:04  rschoene
 *  execute is shown only in unix-systems
 *
 *  Revision 1.16  2005/05/23 08:07:46  rschoene
 *  check whether to save files when selecting another kernel
 *
 *  Revision 1.15  2005/05/10 11:01:32  wloch
 *  fixed spacing error in tooltip
 *
 *  Revision 1.14  2005/04/21 06:49:26  rschoene
 *  removed bug, that set the first selected sorting in the Menu to Language in every case
 *
 *  Revision 1.13  2005/04/19 15:22:19  wloch
 *  implemented sorting in result tree
 *
 *  Revision 1.12  2005/04/14 16:11:57  wloch
 *  implemented sorting of the kernel tree
 *
 *  Revision 1.11  2005/02/22 11:41:01  wloch
 *  corrected remote execute menu entry in menu and popup
 *
 *  Revision 1.10  2005/02/21 19:13:16  wloch
 *  major changes to menu structure and popup menus
 *
 *  Revision 1.9  2005/02/18 09:45:19  wloch
 *  implemented a better popup menu API
 *
 *
 ******************************************************************************/

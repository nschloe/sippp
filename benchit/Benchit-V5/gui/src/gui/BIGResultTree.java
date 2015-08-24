/*********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGResultTree.java
 *
 *  Author: Robert Wloch
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.27 $
 *  $Date: 2007/07/10 12:25:52 $
 *
 *********************************************************************/
package gui;

import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetAdapter;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import plot.BIGOutputFile;
import system.BIGFileHelper;
import system.BIGInterface;
import system.BIGKernel;
import system.BIGZipFileFilter;

/**
 * The BIGResultTree provides neccessary functionality for
 * the results an their tree view.
 **/
public class BIGResultTree extends JTree /*implements BIGPopupRuler*/ {
    Cursor standard = null;
    private BIGGUI gui;
    // is used by inner class (popup), and
    private boolean text = true;
    /**
     * Buffer holding the BRM leave a popup was requested for until a new popup
     * is requested.
     **/
    private String brmLeaveName;
    /** Reference to the BIGInterface. */
    private BIGInterface bigInterface = BIGInterface.getInstance();
    /**
     * The constructor.
     * @param newModel the TreeModel used in the tree.
     * @param gui the BIGGUI this tree is added to
     **/
    public BIGResultTree(TreeModel newModel, BIGGUI gui) {
        super(newModel);
        this.createToolTip();
        this.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
            	// System.out.println("Mouse-Event: " + evt.toString() );
            	if ( isMyPopupTrigger( evt ) ) {
                // if (evt.getButton() != MouseEvent.BUTTON1) {
            	// it's an attempt to display context menu on mac
            	// evt.getModifiers()== 4 -> redmond system
            	// evt.getModifiers()== 18 -> mac
            	// if ( (evt.getModifiers()==4) || (evt.getModifiers()==18) ) {
            	// if (evt.isPopupTrigger()) {
            		JPopupMenu popUp = computePopup(evt);
                    popUp.setInvoker(BIGResultTree.this);
                    popUp.setRequestFocusEnabled(true);
                    popUp.setLocation(evt.getX() +
                                      (int) BIGResultTree.this.getLocationOnScreen().
                                      getX(),
                                      evt.getY() +
                                      (int) BIGResultTree.this.getLocationOnScreen().
                                      getY());
                    popUp.setVisible(true);
                    popUp.revalidate();
                    // add(popUp);
                }
            }
        });

        this.gui = gui;

        // get the initial sorting from BGUI.cfg
        try {
            newSorting = bigInterface.getBIGConfigFileParser().
                         intCheckOut("lastResultSortOrder");
        } catch (Exception e) {
            newSorting = 0;
        }
       plot.BIGResultMixer[] mixers= BIGResultTree.this.gui.getResultMixer();
       //System.err.println(mixers);
       for (int i=0;i<mixers.length;i++)
       {
       //System.err.println("i:"+mixers[i]);
          mixers[ i ].setResultTree( this ) ;
       }
        this.addDragNDrop();
    }

    /**
     * remove a Node
     * @param lastPathComp DefaultMutableTreeNode the node to remove
     * @param ask boolean ask whether to delete the files
     * @return boolean succesfully removed?
     */
    private boolean removeNode(DefaultMutableTreeNode lastPathComp, boolean ask)  {
        if (lastPathComp.getUserObject() instanceof BIGOutputFile) {
            BIGOutputFile file = (BIGOutputFile) lastPathComp.getUserObject();
            String fileName = file.getFile().
                              getAbsolutePath();
            // get the name of the file without any extensions
            int end = fileName.indexOf(".");
            if (end < 1) {
               System.err.println("File has wrong name");
                return false;
            }
            String shortName = file.getFile().getName();
            // show last chance before deleting dialog
            if (ask)
            {
               if ( JOptionPane.showConfirmDialog(
                   BIGResultTree.this.gui ,
                   "Do you really want to delete the files of the selected measurements?"
                   , "Delete results" ,
                   JOptionPane.YES_NO_OPTION ) == JOptionPane.NO_OPTION )
               {
                  return false;
               }
            }
            int deleted = 0;
            if (!((BIGOutputFile) file).getFile().delete())
                System.err.println("An Error occured "
                                   +
                                   "while deleting the result file: " +
                                   fileName);
            else deleted++;
            if ((new File(fileName + ".gp")).exists())
                if (!(new File(fileName + ".gp")).delete())
                    System.err.println("An Error occured "
                                       +
                                       "while deleting the result file: " +
                                       fileName + ".gp");
                 else deleted++;
             if ((new File(fileName + ".gui")).exists())
                 if (!(new File(fileName + ".gui")).delete())
                     System.err.println("An Error occured "
                                        +
                                        "while deleting the result file: " +
                                        fileName + ".gui");
                 else deleted++;

            if ((new File(fileName + ".gp.eps")).exists())
                if (!(new File(fileName + ".gp.eps")).delete())
                    System.err.println("An Error occured "
                                       +
                                       "while deleting the result file: " +
                                       fileName + ".gp.eps");
                else deleted++;
            if (deleted > 0) {
                System.out.println("The files for \"" + shortName +
                                   "\" were deleted!");
               System.out.flush();
            }
            // remove the result node in the tree
            DefaultMutableTreeNode parent = ((DefaultMutableTreeNode) (
                    lastPathComp.getParent()));
            parent.remove(lastPathComp);
            BIGResultTree.this.updateResultTree(parent);
            while (parent.getChildCount() == 0) {
                lastPathComp = parent;
                parent = ((DefaultMutableTreeNode) (
                        lastPathComp.getParent()));
                if (parent == null)
                    return false;
                parent.remove(lastPathComp);
                BIGResultTree.this.updateResultTree(parent);
            }
            return true;
        }
        if ( ( ( DefaultMutableTreeNode ) ( lastPathComp.getParent() ) ).
             getUserObject() instanceof plot.BIGResultMixer )
        {
           plot.BIGResultMixer brm = ( plot.BIGResultMixer ) ( (
               DefaultMutableTreeNode ) ( lastPathComp.getParent() ) ).
                                      getUserObject();
            brm.removeSubNode(lastPathComp.toString());
        }
        return true;
    }

    /**
     * Sets the sorting field of the tree to the given value.
     * Valid values are listed in the Fields section of this class'
     * documentation and are subject to change. An invalid
     * sorting flag will default to BIGResultTree.SORT_BY_NAME.
     * If the new sorting is different than the current one the tree
     * will be updated, otherwise nothing will happen.
     *
     * @param newSorting The new sort order for the result tree.
     **/
    private void setSorting(int newSorting) {

        this.newSorting = newSorting;
        bigInterface.getBIGConfigFileParser().set(
                "lastResultSortOrder", "" + newSorting);
    }

    /**
     * This is called by the BIGKernelTree to tell the result tree a change
     * of sorting state. The BIGResultTree will ignore the kernel tree's new
     * sorting as long as it's not in the SORT_BY_KERNEL_TREE sorting state.
     **/
    public void kernelSortingChanged() {
        BIGResultTree.this.updateResultTree(null);
    }

    /**
     * Initital setup of the kernel tree. Should be called only once.
     * Use updateKernelTree for later updates and reloads.
     **/
    public void setupTree() {
        // get sort order before last GUI exit
        try {
            newSorting = bigInterface.getBIGConfigFileParser().
                         intCheckOut("lastResultSortOrder");
        } catch (Exception e) {
            newSorting = 0;
        }
        setRootVisible(false);
        setEditable(false);
        setShowsRootHandles(true);
        setCellRenderer(new ResultTreeCellRenderer(BIGResultTree.this.gui));
        //setExpandsSelectedPaths( true );
        DefaultTreeSelectionModel selectionModel =
                new DefaultTreeSelectionModel();
        selectionModel.setSelectionMode(selectionModel.
                                        DISCONTIGUOUS_TREE_SELECTION);
        setSelectionModel(selectionModel);
        // fills the tree and returns the path to the root node
        TreePath path = fillTree();
        updateUI();
    }

    /**
     * Selects the BRM, updates the tree and forces display of BRM.
     **/
    public void showBRM(plot.BIGResultMixer brm) {
        // updateResultTree() ;
        BIGResultTree.this.gui.setResultMixer(brm);
    }

    public void removePlotInformations()
    {
       Vector v = new Vector() ;
       readOutputFiles( v ,
                        new File( system.BIGInterface.getInstance().getBenchItPath() +
                                  File.separator + "output" ) ) ;
       for (int i=0;i<v.size();i++)
          ((BIGOutputFile)v.get(i)).removeSavedInformations();
    }
    /**
     * Updates the result tree.
     * @param node the node to update
     **/
    public void updateResultTree(TreeNode node) {
        /*   (new Exception()).printStackTrace();
           Thread t = new Thread()
           {
              public void run()
              {
                 fillTree() ;
              }
           } ;
           SwingUtilities.invokeLater( t ) ;*/

        DefaultTreeModel model = ( ( DefaultTreeModel ) getModel() ) ;
           DefaultMutableTreeNode rootNode =
               ( DefaultMutableTreeNode ) ( model.getRoot() ) ;

           Vector mixer = new Vector() ;
           if ((node==null)||(node==rootNode))
           for ( int i = 0 ; i < rootNode.getChildCount() ; i++ )
           {
              if ( ( ( DefaultMutableTreeNode ) rootNode.getChildAt( i ) ).
                   getUserObject() instanceof plot.BIGResultMixer )
              {
                 mixer.add( ( ( DefaultMutableTreeNode ) rootNode.getChildAt( i ) ).
                            getUserObject() ) ;
                 rootNode.remove( i ) ;
                 i-- ;
              }
           }

        if ( node == null )
        {
           File outputDir = new File( bigInterface.getBenchItPath()
                                      + File.separator + "output" ) ;


           this.revalidate() ;
           this.buildTree( rootNode , outputDir ) ;
        //   return ;
     }
     if ( ( node == null ) || ( node == rootNode ) )
        for ( int i = 0 ; i < mixer.size() ; i++ )
        {
           rootNode.add( ( ( plot.BIGResultMixer ) mixer.get( i ) ).
                         getTreeNode() ) ;
        }
     this.revalidate() ;
     model.reload( node ) ;
  }
    /**
     * Fills the tree's model with content.
     *
     * @return the TreePath to the root node.
     **/
    private TreePath fillTree() {
        DefaultTreeModel model = (DefaultTreeModel) getModel();
        DefaultMutableTreeNode rootNode =
                (DefaultMutableTreeNode) (model.getRoot());
        rootNode.removeAllChildren();
        File outputDir = new File(bigInterface.getBenchItPath()
                                  + File.separator + "output");

        this.buildTree(rootNode, outputDir);
        model.setRoot(rootNode);
        /*
               TreeMap resultTreeMap = getResultMap();
               TreeSet keys = new TreeSet( resultTreeMap.keySet() );
               Iterator iter = keys.iterator();
               addResultsToTree( model, rootNode, resultTreeMap, iter );
               if ( rootNode != null ) {
         rootNode.add( BIGResultTree.this.gui.getResultMixer().getTreeNode() );
               }*/
        return new TreePath(model.getPathToRoot(rootNode));
    }
    /**
     * Converts the map's leaves to TreeSets and inserts them under the leave's
     * toString() name into the map. The old leave Objects are removed. The
     * results ArrayList must contain result filenames according to BenchIT
     * conventions (see www.benchit.org/forum). The deepest kernel directory name
     * will be extracted from that file name nad is used to insert the file into
     * the correct TreeSet according to the TreeSet's key name.
     * This is a recursive method.
     *
     * @param from The TreeMap as returned from BIGKernelTree.getCurrentTreeMap().
     * @param to The TreeMap to build out of the from Map.
     * @param results An ArrayList of result (*.bit) file names.
     * @return the number of result files in the map.
     * @deprecated
     **/
    private int transformAndFillResultTree(Map from, Map to,
                                           ArrayList results) {
        int nResults = 0;
        Iterator it = from.keySet().iterator();
        while (it.hasNext()) {
            String key = (String) it.next();
            Object value = from.get(key);
            if (value instanceof Map) {
                // recurse
                Map toValue = new TreeMap();
                int num = transformAndFillResultTree((Map) value, toValue,
                        results);
                if (num > 0) {
                    // result files are in the Map toValue
                    // add sub maps if they are not empty
                    to.put(new String(key), toValue);
                    nResults += num;
                }
            } else if (value instanceof Set) {
                // replace the TreeSet with a TreeMap
                Set oldSet = (Set) value;
                // add TreeSets
                Iterator it2 = oldSet.iterator();
                while (it2.hasNext()) {
                    Object elem = it2.next();
                    int numResults = 0;
                    if (elem instanceof BIGKernel) {
                        String subKey = ((BIGKernel) elem).getName();
                        Set subValue = new TreeSet();
                        // add results to the TreeSet, if there are any
                        for (int i = 0; i < results.size(); i++) {
                            String name = (String) results.get(i);
                            // according to BenchIT result file name conventions
                            // the directory name of the kernel the file belongs
                            // to is the first part of the file name up to the
                            // 5th underline character before the double
                            // underline occurance
                            int end = name.indexOf("__");
                            // ignore wrong result files
                            if (end < 0) {
                                System.err.println("ignoring result (__): " +
                                        name);
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 1st
                            if (end < 0) {
                                System.err.println("ignoring result (_1): " +
                                        name);
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 2nd
                            if (end < 0) {
                                System.err.println("ignoring result (_2): " +
                                        name);
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 3rd
                            if (end < 0) {
                                System.err.println("ignoring result (_3): " +
                                        name);
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 4th
                            if (end < 0) {
                                System.err.println("ignoring result (_4): " +
                                        name);
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 5th
                            if (end < 0) {
                                System.err.println("ignoring result (_5): " +
                                        name);
                                continue;
                            }
                            // Last, most none java kernels may have a number attached
                            // to the directory name without any special separator
                            // character. This number is called SUBSCRIBER in the
                            // benchit.c source code, however, no documentation exists
                            // about it. The only thing that's certain is, that it is
                            // an integer. With this info we assume
                            String dirName = name.substring(0, end);
                            if (subKey.compareTo(dirName) == 0) {
                                // a result file belongs to this kernel
                                // -> add it to this TreeSet
                                subValue.add(name);
                                // increase number of results located in this map
                                numResults++; // sum of this last directory
                            } else {
                                System.err.println("subKey=" + subKey +
                                        ", dirName="
                                        + dirName + ", result not added: " +
                                        name);
                            }
                        }
                        // only add this node, if it contains result files
                        if (numResults > 0) {
                            to.put(subKey, subValue);
                        }
                        nResults += numResults; // total sum
                    }
                }
            }
        }
        return nResults;
    }

    /**
     * Adds the results to the TreeSet found in their mapping. If there is no
     * mapping for a result a node with the name "unmatched" will be created, if
     * it doesn't exist yet, and the result is added to it's TreeSet.
     * @deprecated
     **/
    private void fillResultTree(Map resultMap, Map results) {
        if (resultMap.isEmpty() || results.isEmpty())return;
        // TreeSet for results without any match
        TreeSet unmatched = new TreeSet();
        // iterate over the result files
        Iterator it = results.keySet().iterator();
        while (it.hasNext()) {
            String fileName = (String) it.next();
            // extract their directory maps
            Map dirMap = (Map) results.get(fileName);
            if (dirMap.isEmpty()) {
                // results without any match are added to unmatched TreeSet
                unmatched.add(fileName);
            } else {
                // get the result's matching TreeMap
                Iterator it2 = dirMap.keySet().iterator();
                while (it2.hasNext()) {
                    String dirName = (String) it2.next();
                    Object obj = dirMap.get(dirName);
                    // in case of an exact match the value would be a String
                    if (obj instanceof TreeSet) {
                        TreeSet set = (TreeSet) obj;
                        // add the result file's name to this TreeMap
                        set.add(fileName);
                    }
                }
            }
        }
        // finally add a mapping for unmatched results
        if (!unmatched.isEmpty()) {
            resultMap.put("unmatched", unmatched);
        }
    }

    /**
     * Converts the map's leaves to TreeSets and inserts them under the leave's
     * toString() name into the map. The old leave Objects are removed. The
     * results Map must contain result filenames according to BenchIT
     * conventions (see www.benchit.org/forum) as keys. The values are
     * Maps <String, TreeSet> with Strings being the real directory names as
     * found in from-Map and the TreeSets being the sets of bit-files for the
     * directory's node.<br>
     * The deepest kernel directory name will be extracted from that file name
     * and is used to determine the file's correct TreeSet according to
     * the TreeSet's key name. Sometimes, especially for none-Java kernels no
     * exact mappings are possible without looking into the bit files. The
     * directory with the closest match will be used in this case.
     * Not matchable kernels will an empty Map as value. The
     * results Map must be used to add the kernels after the first call of this
     * method returns.
     *
     * This is a recursive method.
     *
     * @param from The TreeMap as returned from BIGKernelTree.getCurrentTreeMap().
     * @param to The TreeMap to build out of the from Map.
     * @param results A Map of result (*.bit) file names.
     * @return the number of result files in the map.
     **/
    private int transformResultTree(Map from, Map to, Map results) {
        int nResults = 0;
        Iterator it = from.keySet().iterator();
        while (it.hasNext()) {
            String key = (String) it.next();
            Object value = from.get(key);
            if (value instanceof Map) {
                // recurse
                Map toValue = new TreeMap();
                int num = transformResultTree((Map) value, toValue, results);
                if (num > 0) {
                    // result files are in the Map toValue
                    // add sub maps if they are not empty
                    to.put(new String(key), toValue);
                    nResults += num;
                }
            } else if (value instanceof Set) {
                // replace the TreeSet with a TreeMap
                Set oldSet = (Set) value;
                // add TreeSets
                Iterator it2 = oldSet.iterator();
                while (it2.hasNext()) {
                    Object elem = it2.next();
                    int numResults = 0;
                    if (elem instanceof BIGKernel) {
                        String subKey = ((BIGKernel) elem).getName();
                        Set subValue = new TreeSet();
                        // add results to the TreeSet, if there are any
                        Iterator resIt = results.keySet().iterator();
                        while (resIt.hasNext()) {
                            String name = (String) resIt.next();
                            // according to BenchIT result file name conventions
                            // the directory name of the kernel the file belongs
                            // to is the first part of the file name up to the
                            // 5th underline character before the double
                            // underline occurance
                            int end = name.indexOf("__");
                            // ignore wrong result files
                            if (end < 0) {
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 1st
                            if (end < 0) {
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 2nd
                            if (end < 0) {
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 3rd
                            if (end < 0) {
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 4th
                            if (end < 0) {
                                continue;
                            }
                            end = name.lastIndexOf("_", end - 1); // 5th
                            if (end < 0) {
                                continue;
                            }
                            String dirName = name.substring(0, end);
                            if (subKey.compareTo(dirName) == 0) {
                                // a result file belongs definitely to this kernel
                                Map dirMap = (Map) results.get(name);
                                // remove any previous mappings
                                dirMap.clear();
                                // -> set this TreeSet and directory in the results Map
                                dirMap.put(subKey, subValue);
                                // mark this result as an exect match,
                                // this prevents the mapping from being replaced on
                                // later unexact matches
                                dirMap.put("exact", name);
                                // increase number of results located in this map
                                numResults++; // sum of this last directory
                            } else {
                                Map dirMap = (Map) results.get(name);
                                // only "try" to match a result, if an exact match
                                // was not found yet
                                if (!dirMap.containsKey("exact")) {
                                    // we found a rough match here
                                    if (dirName.startsWith(subKey)) {
                                        // calculate the exactness of the new match
                                        int diff = dirName.length() -
                                                subKey.length();
                                        // check, if there's a previous match
                                        if (dirMap.size() == 1) {
                                            Iterator matchIt = dirMap.keySet().
                                                    iterator();
                                            while (matchIt.hasNext()) {
                                                String matchKey = (String)
                                                        matchIt.next();
                                                // calculate that exactness
                                                int lastDiff = dirName.length() -
                                                        matchKey.length();
                                                // check if new match is more exact
                                                if (lastDiff > diff) {
                                                    // remove old match
                                                    dirMap.clear();
                                                    // add match
                                                    dirMap.put(subKey, subValue);
                                                }
                                                break;
                                            }
                                        } else {
                                            // there is no previous match, so add this one
                                            // just to make sure
                                            dirMap.clear();
                                            // add match
                                            dirMap.put(subKey, subValue);
                                            // increase number of results located in this map
                                            numResults++; // sum of this last directory
                                        }
                                    }
                                }
                            }
                        }
                        // only add this node, if it contains result files
                        if (numResults > 0) {
                            to.put(subKey, subValue);
                        }
                        nResults += numResults; // total sum
                    }
                }
            }
        }
        return nResults;
    }

    /**
     * Method to add all results to the result tree. Recursive if sorting
     * by kernel tree.
     *
     * @param model The model of the tree.
     * @param parentNode The parent node for new nodes.
     * @param parent The map representing the tree structure.
     * @param it The iterator over the map's keys.
     * @deprecated
     **/
    private void addResultsToTree(DefaultTreeModel model,
                                  DefaultMutableTreeNode parentNode,
                                  Map parent,
                                  Iterator it) {
        while (it.hasNext()) {
            DefaultMutableTreeNode childNode = null;
            String keyName = (String) it.next();
            Object value = parent.get(keyName);
            if (value instanceof Map) {
                // create node
                childNode = new DefaultMutableTreeNode(keyName);
                model.insertNodeInto(
                        childNode, parentNode, parentNode.getChildCount());
                // recurse
                Iterator it2 = ((Map) value).keySet().iterator();
                addResultsToTree(model, childNode, (Map) value, it2);
            } else if (value instanceof Set) {
                Set ts = (Set) parent.get(keyName);
                Iterator it2 = ts.iterator();
                childNode = new DefaultMutableTreeNode(keyName);
                model.insertNodeInto(
                        childNode, parentNode, parentNode.getChildCount());
                while (it2.hasNext()) {
                    String name = (String) it2.next();
                    DefaultMutableTreeNode ccNode = new DefaultMutableTreeNode(
                            name);
                    model.insertNodeInto(
                            ccNode, childNode, childNode.getChildCount());
                    BIGResultTree.this.gui.getResults().add(name);
                }
            }
        }
    }

    private int getNumberOfSubOutputFiles(DefaultMutableTreeNode node) {
        int files = 0;
        for (int i = 0; i < node.getChildCount(); i++) {
            if (node.getChildAt(i).isLeaf())
                files++;
            else
                files = files +
                        getNumberOfSubOutputFiles((DefaultMutableTreeNode) node.
                                                  getChildAt(i));

        }
        return files;
    }

    /**
     * Sets up a TreeSelectionListener and a MouseListener and adds them
     * to the tree.
     **/
    public void setListeners() {
        this.addTreeExpansionListener(new TreeExpansionListener() {
            private void expandAll(JTree tree, TreePath parent, boolean expand) {
                // Traverse children
                TreeNode node = (TreeNode) parent.getLastPathComponent();
                if (node.getChildCount() >= 0) {
                    for (Enumeration e = node.children(); e.hasMoreElements(); ) {
                        TreeNode n = (TreeNode) e.nextElement();
                        TreePath path = parent.pathByAddingChild(n);
                        expandAll(tree, path, expand);
                    }
                }

                // Expansion or collapse must be done bottom-up
                if (expand) {
                    tree.expandPath(parent);
                } else {
                    tree.collapsePath(parent);
                }
            }

            public void treeExpanded(TreeExpansionEvent tEvt) {
                TreePath path = tEvt.getPath();
                DefaultMutableTreeNode lastNode = (DefaultMutableTreeNode)
                                                  path.getLastPathComponent();
                if (getNumberOfSubOutputFiles(lastNode) < 8) {
                    expandAll(BIGResultTree.this, path, true);
                }
            }

            public void treeCollapsed(TreeExpansionEvent tEvt) {

            }

        });
        // adds tree selection listener for view display change
        addTreeSelectionListener(new TreeSelectionListener() {
            Thread thread = null;
            public void valueChanged(TreeSelectionEvent tse) { // called on selection change only
                if (gui.getState() != gui.RESULTS) {
                    gui.setGUIState(gui.RESULTS);
                }
                TreePath path = null;
                if (tse.isAddedPath()) {
                    // path was added to the selection
                    path = tse.getPath();
                }
                if (path == null) {
                    return;
                }
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.
                                              getLastPathComponent();
                final Object obj = node.getUserObject();
                //!!System.err.println("UserObject:"+obj.toString());
                if (obj instanceof String) {
                    // node
                } else {
                    //leaf
                    if (obj instanceof BIGOutputFile) {
                        thread = new Thread() {
                           public void run()
                           {
                              BIGResultTree.this.gui.loadResult( (
                                  BIGOutputFile ) obj,true ) ;
                           }
                        };
                        thread.start();

                    } else { // BRM stuff
                        if (obj instanceof plot.BIGResultMixer) {
                            BIGResultTree.this.gui.setResultMixer((plot.BIGResultMixer)obj);
                            // BRM node

                         }
                         else
                         {
                            if ( obj instanceof conn.Graph )
                            {
                            	// TODO: Abfangen der NullPointerException
                               BIGResultTree.this.gui.setResultMixer( ( plot.BIGResultMixer ) ( (
                                   DefaultMutableTreeNode ) node.getParent() ).
                                  getUserObject() ) ;
                              // BRM subnode

                            } else {
                                System.err.println(
                                        "Unknown node in Resulttree:\n" +
                                        obj);
                            }

                        }
                    }
                }

/*                String name = obj.toString();
                if (!BIGResultTree.this.gui.getResults().contains(name)
                    && (!name.startsWith("Mixer"))) {
                    // no result file selected, but a parent node
                    path = null;
                }
                if (path != null) {
                    if (thread != null && thread.isAlive()) {
                        try {
                            // if last result is still loading we wait until
                            // it's finished loading
                            thread.join();
                        } catch (InterruptedException ie) {}
                    }
                    // load new result plot
                    final String nodeName = name;
                    thread = new Thread() {
                        public void run() {
                            if (nodeName.startsWith("Mixer")) {
                                BIGResultTree.this.gui.setResultMixer();
                            } else {
                                BIGResultTree.this.gui.loadResult(null);
                            }
                        }
                    };
                    thread.start();
                }*/
            }
        }); // end of addTreeSelectionListener()

        // adds mouse listener for pop up menu
        /*addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                if (gui.getState() != gui.RESULTS) {
                    gui.setGUIState(gui.RESULTS);
                }
            }
        });*/
        // JTree MUST be registered manually to display tool tips
        ToolTipManager.sharedInstance().registerComponent(this);
   /*     addMouseMotionListener(new MouseMotionAdapter() {
            public void mouseMoved(MouseEvent e) {
               System.err.println(e);
               JToolTip tt=
               BIGResultTree.this.createToolTip();
           setToolTipText(computeToolTipText(e));
               tt.setTipText(computeToolTipText(e));
               System.err.println(tt.getTipText());
               tt.setComponent(BIGResultTree.this);
               tt.setLocation(e.getPoint());
               tt.setVisible(true);

            }
        });*/
    }

    public void updateBRM(plot.BIGResultMixer brm) {
        DefaultTreeModel model = (DefaultTreeModel) getModel();
        DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode) model.
                                          getRoot();

        model.nodeChanged(brm.
                          getTreeNode());
        model.nodeChanged(rootNode);
        ((DefaultTreeModel) getModel()).reload(brm.
                                               getTreeNode());
        gui.setResultMixer(brm);
    }

    /**
     * Removes a node in the JTree using it's model.
     * @param name The String identifying the TreeNode's user object.
     * @param selectedPath The TreePath containing the TreeNode.
     * @deprecated
     **/
    private void removeNode(Object file, TreePath selectedPath) {
        DefaultTreeModel model = (DefaultTreeModel) getModel();
        DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode) model.
                                          getRoot();
        // find tree node of selected node in the tree
        boolean nodeFound = false;
        DefaultMutableTreeNode parent = rootNode;
        for (int k = 1; k < selectedPath.getPathCount(); k++) {
            Object pathObj = selectedPath.getPathComponent(k);
            if (pathObj == null) {
                break;
            }
            int num = parent.getChildCount();
            boolean childFound = false;
            for (int l = 0; l < num; l++) {
                DefaultMutableTreeNode childNode =
                        (DefaultMutableTreeNode) parent.getChildAt(l);
                if (pathObj.toString().compareTo(childNode.toString()) == 0) {
                    childFound = true;
                    if (file == (childNode.getUserObject())) {
                        // and delete it
                        model.removeNodeFromParent(childNode);
                        DefaultMutableTreeNode remNode = parent;
                        // delete parent nodes, if they have no more children
                        while ((remNode.getChildCount() == 0) &&
                               remNode != rootNode) {
                            DefaultMutableTreeNode tmpNode =
                                    (DefaultMutableTreeNode) remNode.getParent();
                            model.removeNodeFromParent(remNode);
                            remNode = tmpNode;
                        }
                        nodeFound = true;
                    }
                    parent = childNode;
                    break;
                }
            }
            if (!childFound || nodeFound) {
                break;
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
    public DefaultMutableTreeNode findNode(TreePath selectedPath) {
        DefaultMutableTreeNode result = null;
        String nodeName = selectedPath.getLastPathComponent().toString();
        DefaultTreeModel model = (DefaultTreeModel) getModel();
        DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode) model.
                                          getRoot();
        // find tree node of selected node in the tree
        boolean nodeFound = false;
        DefaultMutableTreeNode parent = rootNode;
        for (int k = 1; k < selectedPath.getPathCount(); k++) {
            Object pathObj = selectedPath.getPathComponent(k);
            if (pathObj == null) {
                break;
            }
            int end = parent.getChildCount();
            boolean childFound = false;
            for (int l = 0; l < end; l++) {
                DefaultMutableTreeNode childNode =
                        (DefaultMutableTreeNode) parent.getChildAt(l);
                if (pathObj.toString().compareTo(childNode.toString()) == 0) {
                    childFound = true;
                    parent = childNode;
                    if (nodeName.compareTo(childNode.toString()) == 0) {
                        result = childNode;
                        nodeFound = true;
                    }
                    break;
                }
            }
            if (!childFound || nodeFound) {
                break;
            }
        }
        return result;
    }

    /**
     * Returns null, or the name of the BRM sub node in the result tree,
     * that was just performed a popup request on.
     * @deprecated
     **/
    public String getBRMLeaveName() {
        return brmLeaveName;
    }

    /**
     * This method should return true if a popup menu should be
     * shown for a certain TreeNode or component.
     * @param me The MouseEvent trying to invoke the popup menu.
     * @param name The identifier name of the BIGPopupMenu trying to popup.
     * @return a popup is shown
     **/
    public boolean showPopup(MouseEvent me, String name) {
        boolean retval = false;
        brmLeaveName = null;
        if (name.compareTo("ResultPopup") == 0) {
            TreePath selPath = getPathForLocation(me.getX(), me.getY());
            // if the path expander is clicked, no path is selected
            if (selPath != null) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath.
                                              getLastPathComponent();
                if (node.getUserObject() instanceof BIGOutputFile) {
                    retval = true;
                }
            }
        } else if (name.compareTo("BRMPopup") == 0) {
            TreePath selPath = getPathForLocation(me.getX(), me.getY());
            // if the path expander is clicked, no path is selected
            if (selPath != null) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath.
                                              getLastPathComponent();
                if (node.getUserObject() instanceof plot.BIGResultMixer) {
                    retval = true;
                }
            }
        } else if (name.compareTo("BRMSubPopup") == 0) {
            TreePath selPath = getPathForLocation(me.getX(), me.getY());
            // if the path expander is clicked, no path is selected
            if (selPath != null) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath.
                                              getLastPathComponent();
                if (node.getUserObject() instanceof conn.Graph) {
                    this.brmLeaveName = node.toString();
                    retval = true;
                }

            }
        } else if (name.compareTo("SortPopup") == 0) {
            TreePath selPath = getPathForLocation(me.getX(), me.getY());
            // the sort only popup is only shown, if all the others can't be shown.
            if (selPath != null) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath.
                                              getLastPathComponent();
                if (node.getUserObject() instanceof String) {
                    retval = true;
                }
                /*
                            Object obj = selPath.getLastPathComponent();
                 Object parent = selPath.getPathComponent( selPath.getPathCount() -
                                    2 );
                            if ( obj == null ) {
                               retval = true;
                            }
                 else if ( !this.gui.getResults().contains( obj.toString() ) &&
                                      !obj.toString().startsWith( "BRM" ) ) {
                               if ( parent == null ) {
                                  retval = true;
                               }
                 else if ( !parent.toString().startsWith( "BRM" ) ) {
                                  retval = true;
                               }
                            }*/
            }
        }
        return retval;
    }

    /**
     * The result tree's cell renderer. This class chooses the
     * appropriate icons for tree nodes and displays tool tip
     * texts if the mouse cursor is above a tree node.
     **/
    class ResultTreeCellRenderer extends DefaultTreeCellRenderer {
        ImageIcon resultIcon;
        ImageIcon resultBaseOpenedIcon;
        ImageIcon resultBaseClosedIcon;
        ImageIcon onlineMixerIcon;
        BIGGUI big;
        /**
         * Constructs a new tree cell renderer.
         * @param gui the gui where this renderers tree is added to
         **/
        public ResultTreeCellRenderer(BIGGUI gui) {
            big = gui;
            resultIcon = new ImageIcon(bigInterface.getBenchItPath()
                                       + File.separator + "gui" +
                                       File.separator +
                                       "img"
                                       + File.separator + "result16.png");
            resultBaseOpenedIcon = new ImageIcon(bigInterface.getBenchItPath()
                                                 + File.separator + "gui" +
                                                 File.separator + "img"
                                                 + File.separator +
                                                 "result_opened16.png");
            resultBaseClosedIcon = new ImageIcon(bigInterface.getBenchItPath()
                                                 + File.separator + "gui" +
                                                 File.separator + "img"
                                                 + File.separator +
                                                 "result_closed16.png");
            onlineMixerIcon = new ImageIcon(bigInterface.getBenchItPath()
                                                 + File.separator + "gui" +
                                                 File.separator + "img"
                                                 + File.separator +
                                                 "webmixer.png");

        }

        /**
         * Overriding super class' method.
         **/
        public java.awt.Component getTreeCellRendererComponent(JTree tree,
                Object value, boolean sel, boolean expanded, boolean leaf,
                int row,
                boolean hasFocus) {
            super.getTreeCellRendererComponent(
                    tree, value, sel, expanded, leaf, row, hasFocus);
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
            if (node.getUserObject() instanceof plot.BIGOnlineMixer)
            {
               setIcon(onlineMixerIcon);
               return this;
            }
            setIcon(resultIcon); // default node icon
            if (node.equals(node.getRoot())) {
                // set icon for root node although it's not visible
                setIcon(resultBaseOpenedIcon);
            } else if (!node.isLeaf()) {
                // set icons for parent nodes
                if (expanded) {
                    setIcon(resultBaseOpenedIcon);
                } else {
                    setIcon(resultBaseClosedIcon);
                }
             }
            return this;
        }

        /*   private int countLeaves( TreeNode node ) {
              int retval = 0;
              int count = node.getChildCount();
              if ( count > 0 ) {
                 for ( int i = 0; i < count; i++ ) {
                    TreeNode child = node.getChildAt( i );
                    if ( child.isLeaf() ) {
                       if ( ((DefaultMutableTreeNode)child).getUserObject().toString().endsWith( ".bit" ) ) {
                          retval++;
                       }
                    }
                    else {
                       retval += countLeaves( child );
                    }
                 }
              }
              return retval;
           }*/
    }


    /**
     * (re)builds this' content
     * @param rootNode DefaultMutableTreeNode the node, where to add the results from the directory
     * @param directory File the directory which is checked for result
     */
    public void buildTree(
            final DefaultMutableTreeNode rootNode,
            final File directory) {
        Vector outputFilesVec = new Vector();
        readOutputFiles(outputFilesVec, directory);
        // sort and insert kernels
        Vector sortedVec = new Vector();
        // get min
        while (outputFilesVec.size() > 0) {
            int position = 0;
            int min = Integer.MAX_VALUE;
            String lastkernelString = null;

            for (int i = 0; i < outputFilesVec.size(); i++) {
                if (lastkernelString == null) {
                    position = i;
                    lastkernelString = ((BIGOutputFile) outputFilesVec.get(i)).
                                       getFile().getAbsolutePath();
                } else

                if (((BIGOutputFile) outputFilesVec.get(i)).getFile().
                    getAbsolutePath().compareTo(lastkernelString) < 0) {
                    position = i;
                    lastkernelString = ((BIGOutputFile) outputFilesVec.get(i)).
                                       getFile().getAbsolutePath();
                }
            }
            sortedVec.add(outputFilesVec.get(position));
            outputFilesVec.removeElementAt(position);
        }
        for (int i = 0; i < sortedVec.size(); i++) {
            addOutputFile(rootNode, (BIGOutputFile) sortedVec.get(i));
        }

        //BIGResultTree.this.gui.getResultMixer().setResultTree(  BIGResultTree.this );
        plot.BIGResultMixer[] mixers=BIGResultTree.this.gui.getResultMixer();
        for (int i=0;i<mixers.length;i++)
        rootNode.add(mixers[i].getTreeNode());
    }

    /**
     * builds a menu for this tree for sorting
     * @return JMenu the sorting menu
     */
    private JMenu getSortMenu() {
        // if (true)
        // {
        JMenu sortMenu = new JMenu("Sort by");
        JMenuItem item = new JMenuItem("type");
        ActionListener sortListener = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                JMenuItem source = (JMenuItem) evt.getSource();
                //!!System.err.println(source.getText());
                for (int i = 0; i < getSortMenu().getItemCount(); i++) {
                    //!!System.err.println( ( ( JMenuItem ) getSortMenu().getItem( i ) ).
                    //!!     getText()) ;
                    if (((JMenuItem) getSortMenu().getItem(i)).
                        getText().equals(source.getText())) {
                        //!!System.err.println("found");
                        //!!System.err.println("set sorting to "+i);
                        BIGResultTree.this.setSorting(i);
                        BIGResultTree.this.fillTree();
                        break;

                    }
                }
            }
        };
        item.addActionListener(sortListener);
        sortMenu.add(item);
        item = new JMenuItem("name");
        item.addActionListener(sortListener);
        sortMenu.add(item);
        item = new JMenuItem("language");
        item.addActionListener(sortListener);
        sortMenu.add(item);
        item = new JMenuItem("parallelization libraries");
        item.addActionListener(sortListener);
        sortMenu.add(item);
        item = new JMenuItem("other libraries");
        item.addActionListener(sortListener);
        sortMenu.add(item);
        item = new JMenuItem("data type");
        item.addActionListener(sortListener);
        sortMenu.add(item);
        return sortMenu;
        // }
    }

    /**
     * adds an outputfile to a node by adding all the nodes, which are directories
     * as subnodes between and the file as leaf
     * @param rootNode DefaultMutableTreeNode here the output file (with its directories) is added (should be the rootnode)
     * @param outFile BIGOutputFile the file, you want to add
     */
    private void addOutputFile(DefaultMutableTreeNode rootNode,
                               BIGOutputFile outFile) {
        //!!System.err.println("New Sorting:"+this.newSorting);
        String name = outFile.getNameAfterSorting(this.newSorting) + "." +
                      outFile.getFilenameWithoutExtension();
        //!!System.err.println("Output-file:\n"+name);
        DefaultMutableTreeNode tempNode = rootNode;
        StringTokenizer stok = new StringTokenizer(name, ".");
        String part;
        boolean found;
        // first the folders
        for (int j = 0; j < outFile.getSortedNames(0).length; j++) {
            part = stok.nextToken();
            found = false;

            for (int i = 0; i < tempNode.getChildCount(); i++) {
                if (((DefaultMutableTreeNode) tempNode.getChildAt(i)).
                    toString().equals(part)) {
                    tempNode = (DefaultMutableTreeNode) tempNode.getChildAt(i);
                    found = true;
                    break;
                }
            }
            if (!found) {

                DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
                        part);
                tempNode.insert(newNode, tempNode.getChildCount());
                this.updateResultTree(tempNode);
                tempNode = newNode;
            }
        }
        // then the filename
        part = stok.nextToken()+'.';
        while (stok.hasMoreTokens())
           part=part+stok.nextToken()+'.';
        //remove last '.'
        part=part.substring(0,part.length()-1);
        //part=part.substring(0,part.lastIndexOf(".bit"));
        found = false;

        for (int i = 0; i < tempNode.getChildCount(); i++) {
            if (((DefaultMutableTreeNode) tempNode.getChildAt(i)).
                toString().equals(part)) {
                found = true;
                break;
            }
        }
        if (!found) {
            DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(outFile);
            tempNode.insert(newNode, tempNode.getChildCount());
            this.updateResultTree(tempNode);
        }

    }

    // sorting for
    // 0:type
    // 1:name
    // 2:language
    // 3:parallel libraries
    // 4:other libraries
    // 5:data type
    private int newSorting = 0;
    /**
     * reads all the output-files within a directory and adds the BIGResulTree.BIGOutputFile
     * to a Vector
     * @param v Vector will contain the outputfiles within a directory
     * @param directory File the directory where you want to search for output files
     */
    public void readOutputFiles(Vector v, File directory) {
        File[] subFiles = directory.listFiles(new FileFilter() {
            public boolean accept(File f) {
                if (f.isDirectory())
                    if (!f.getName().equals("CVS"))
                        return true;
                if (f.isFile())
                    if (f.getName().endsWith(".bit"))
                        return true;
                return false;
            }
        });
        if (subFiles != null)
            for (int i = 0; i < subFiles.length; i++) {
                if (subFiles[i].isDirectory()) {
                    this.readOutputFiles(v, subFiles[i]);
                } else {
                    v.add(new BIGOutputFile(subFiles[i]));
                }
            }

    }


    /**
     * getDragGestureListener
     *
     * @return DragGestureListener
     */
    private DragGestureListener getDragGestureListener()
    {
       DragGestureListener dgl=new DragGestureListener()
       {
          public void dragGestureRecognized(
              DragGestureEvent e )
          {
             if(e.getComponent()==BIGResultTree.this)
             {
                TreePath tp = ( ( BIGResultTree ) e.getComponent() ).
                    getPathForLocation( ( int ) e.getDragOrigin().getX() ,
                                        ( int ) e.getDragOrigin().getY() ) ;
                if ( tp != null )
                   if ( tp.getLastPathComponent() != null )
                   {
                      Object o = ( ( DefaultMutableTreeNode ) tp.
                                   getLastPathComponent() ).
                          getUserObject() ;
                      if ( o != null )
                         if ( o instanceof BIGOutputFile )
                         {
                            e.startDrag( null ,
                                         ( BIGOutputFile ) o ) ;
                         }
                         else
                         {
                            if ( o instanceof conn.Graph )
                            {
                            	e.startDrag( null, ( (conn.Graph) o ).getCopyOfGraph() );
                               /*e.startDrag( null ,
                                            ( conn.Graph ) o ) ;*/

                            }

                         }
                   }
             }
             else
             {
             }

          }
       };
       return dgl ;
    }

       DropTargetListener dtl =null;

    public DropTargetListener getDropListener()
    {
       if (dtl==null)
          dtl = new DropTargetAdapter()
       {
          public void drop( DropTargetDropEvent evt )
          {
             if (!(evt.getSource() instanceof DropTarget))
                return ;
             if ( ! ( ( ( ( DropTarget ) evt.getSource() ).getComponent() )
                     instanceof BIGResultTree ) )
                return ;

             BIGResultTree tree = ( BIGResultTree ) ( ( DropTarget ) evt.
                 getSource() ).getComponent() ;
             TreePath tp = ( ( BIGResultTree ) tree ).
                 getPathForLocation( ( int ) evt.getLocation().getX() ,
                                     ( int ) evt.getLocation().getY() ) ;
             if ( tp.getLastPathComponent() == null )
             {
                evt.rejectDrop();
                return ;
             }
             Object o = ( ( DefaultMutableTreeNode ) tp.
                          getLastPathComponent() ).getUserObject() ;
             if ( ! ( o instanceof plot.BIGResultMixer ) )
             {
                evt.rejectDrop();
                return ;
             }
             plot.BIGResultMixer brm=(plot.BIGResultMixer)o;
             DataFlavor [] df=evt.getCurrentDataFlavors();
             for (int i=0;i<df.length;i++)
             {
                 o=null;
               try
               {
                  o=evt.getTransferable().getTransferData( df[ 0 ] ) ;
               }
               catch ( IOException ex )
               {
                  System.err.println("Internal Error while DragnDrop (1)");
               }
               catch ( UnsupportedFlavorException ex )
               {
                  System.err.println("Internal Error while DragnDrop (2)");
               }
               if (o!=null)
               {
                  if ( o instanceof BIGOutputFile )
                     brm.showSelectFunctionsDialog(
                         ( ( BIGOutputFile ) o ).getFile() ) ;
                  else
                  if ( o instanceof conn.Graph )
                     brm.addGraph( ( conn.Graph ) o ) ;
                  else
                     evt.rejectDrop();
                  setSelectionPath( tp ) ;
               }
             }
          }
          public void dragEnter( DropTargetDragEvent dropTargetDragEvent )
          {
             dropTargetDragEvent.acceptDrag( DnDConstants.ACTION_COPY_OR_MOVE ) ;
          }
       } ;
       return dtl ;
    }

    private void addDragNDrop()
    {
       DropTarget dt=new DropTarget(this,this.getDropListener());
       this.setDropTarget(dt);
       DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer( this ,
           DnDConstants.ACTION_COPY ,
           getDragGestureListener() ) ;
       this.setDragEnabled(true);
        this.addKeyListener(new KeyAdapter() {
            public void keyTyped(KeyEvent ke) {
                if (ke.getKeyChar() == KeyEvent.VK_DELETE) {
                   delete();
                }
            }

        });
    }
    
    //-------------------------------------------------------------------------------
    /**
     * This method packs all selected BIGOutputFiles in the ResultTree in one zip-file
     *
     */
    private void createZipFile()
    {
    	final String OUTPUTFOLDER = "output";
    	
    	String fileName;
        /*String pathToOutputFolder = BIGInterface.getInstance().getBenchItPath()
        							+ File.separator + "output" + File.separator;*/
    	
        TreePath[] selectedPaths = getSelectionPaths();
    	DefaultMutableTreeNode node;
    	
    	FileInputStream in;
    	FileOutputStream out;
    	File file = getNewOutFile( BIGInterface.getInstance().getLastWorkPath().getAbsolutePath() );
    	BIGOutputFile bigFile;
    	ZipOutputStream zipOut;
    	ZipEntry entry;    	
    	
    	int read = 0;
    	int indexOfOccurence;
    	byte[] data = new byte[1024];
    	
    	if ( file != null ) {
    		BIGInterface.getInstance().setLastWorkPath( file );
        	try
        	{
    			out = new FileOutputStream( file );
    			zipOut = new ZipOutputStream( out );
    			// set compression method for entries
    			zipOut.setMethod(ZipOutputStream.DEFLATED);
    			// if there are selected tree items 
    			if (selectedPaths != null)
    			{
    				// do the following for every selected tree item
    				for (int i = 0; i < selectedPaths.length; i++)
    				{
    					node = (DefaultMutableTreeNode)selectedPaths[i].getLastPathComponent();
    					if ( node.getUserObject() instanceof BIGOutputFile )
    					{
    						bigFile = (BIGOutputFile) node.getUserObject();
    						fileName = bigFile.getFile().getAbsolutePath();
    						// search for "output" in the path of the BIGOutputFile
    						indexOfOccurence = fileName.indexOf(OUTPUTFOLDER);
    						// Was "output" found in the path?
    						if ( indexOfOccurence != -1 ) {
    							// if output was found, then pack the BIGOutputFile relative to
    							// the "output"-location into the zip-file
    							fileName = fileName.substring( indexOfOccurence );
    						}						
    						// System.out.println(fileName);
    						
    						entry = new ZipEntry( fileName );
    						in = new FileInputStream( bigFile.getFile() );
    						
    						zipOut.putNextEntry( entry );
    						while( (read = in.read(data)) > 0 )
    						{
    							zipOut.write(data, 0, read);
    						}
    						zipOut.closeEntry();
    						in.close();
    						
    						setSelectionPath(null);
    					}
    				}				
    			}
    			zipOut.close();
    			out.close();
        	} catch (Exception e) {
    			System.err.println("Error while zipping: abort zipping");
    		}
    	}
    }
    
    /**
     * Opens a JFileChooser to select the zip file
     *  
     * @return newOutFile file the new output file (used to create a zip file later)
     */
    private File getNewOutFile(String path)
    {
    	File newOutputFile = null;
    	JFileChooser jfc = new JFileChooser( path );
    	
    	boolean correctSelection = false;
    	int returnVal, selectedValue;
    	
    	while( correctSelection == false )
    	{
	    	jfc.setFileFilter( new BIGZipFileFilter() );
	    	returnVal = jfc.showSaveDialog(null);
	    	if ( returnVal == JFileChooser.APPROVE_OPTION)
	    	{
	    		newOutputFile = jfc.getSelectedFile();
	        	// System.out.println( newOutputFile.toString() );
	        	if ( !newOutputFile.getName().endsWith(".zip") )
	        	{
	        		newOutputFile = new File( newOutputFile.getAbsolutePath() + ".zip" );
	        	}
	        	if ( newOutputFile.exists() )
	        	{
	        		selectedValue = JOptionPane.showConfirmDialog(null,
	        				"Selected file already exists. Do you wan't to overwrite it?",
	        				"File already exits",
	        				JOptionPane.YES_NO_OPTION);
	        		if (selectedValue == JOptionPane.YES_OPTION)
	        		{
	        			correctSelection = true;
	        		}
	        	}
	        	else
	        	{
	        		correctSelection = true;
	        	}
	    	}
	    	else {
	    		return null;
	    	}
    	}
    	
    	return newOutputFile;
    }
    
    private boolean isMyPopupTrigger(MouseEvent evt) {
    	int macOnMask = InputEvent.META_MASK | InputEvent.BUTTON1_MASK | InputEvent.BUTTON3_MASK;
    	int winOnMask = InputEvent.META_MASK | InputEvent.BUTTON3_MASK;
    	if ( (evt.getModifiers() & macOnMask) == macOnMask ) {
    		return true;
    	}
    	else if ( (evt.getModifiers() & winOnMask) == winOnMask ) {
    		return true;
    	}
    	else return false;
    }
    
    //-------------------------------------------------------------------------------

   private void delete()
    {

       TreePath[] selectedPaths = getSelectionPaths();
       if (selectedPaths != null)
       {
          boolean delete=false;
          // here ask
          DefaultMutableTreeNode node = (
              DefaultMutableTreeNode )
              selectedPaths[ 0 ].getLastPathComponent() ;
          if ( ( node.getUserObject() instanceof BIGOutputFile ) ||
               ( node.getUserObject() instanceof conn.Graph ) )
          {
             delete=removeNode( node, true ) ;
             setSelectionPath( null ) ;
          }
          // now don't ask again
          if ((selectedPaths.length>1)&&(delete))
          for ( int i = 1 ; i < selectedPaths.length ; i++ )
          {
             node = (
                 DefaultMutableTreeNode )
                 selectedPaths[ i ].getLastPathComponent() ;
             if ( ( node.getUserObject() instanceof BIGOutputFile ) ||
                  ( node.getUserObject() instanceof conn.Graph ) )
             {
                removeNode( node ,false) ;
                setSelectionPath( null ) ;
             }
          }
       }

    }
    private JPopupMenu computePopup(MouseEvent evt) {
        final TreePath path = this.getClosestPathForLocation(evt.
                getX(), evt.getY());
        final DefaultMutableTreeNode selectedNode = ((DefaultMutableTreeNode) path.
                                               getLastPathComponent());
        JMenuItem sortMenu = getSortMenu();
        if (selectedNode.getUserObject() instanceof BIGOutputFile) {
            final JPopupMenu pop =  new JPopupMenu();
            JMenuItem item = new JMenuItem("Display");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    BIGResultTree.this.gui.loadResult(null,true);
                }
            });
            pop.add(item);
            
            //---------------------------------------------------------
            item = new JMenuItem("Zip");
            item.addActionListener(
            		new ActionListener() {
            			public void actionPerformed(ActionEvent evt) {
            				createZipFile();            				
            			}
            		});
            pop.add(item);
            //---------------------------------------------------------
            
            //!! wird immer wieder neu erzeugt!!!!!
            item = new JMenuItem("View file as text");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                   System.err.println("view");
                    BIGResultTree.this.gui.loadResult(null,true);
                    if (text) {
                   System.err.println("viewtxt1"+text);
                        BIGResultTree.this.gui.setTextView();
                        ((JMenuItem) ae.getSource()).setText(
                                "View file as chart");
                          } else {
                   System.err.println("viewtxt2"+text);
                        BIGResultTree.this.gui.setGraphView();
                        ((JMenuItem) ae.getSource()).setText(
                                "View file as text");
                     ((JMenuItem)ae.getSource()).revalidate();
                    }
                    text = !text;

                }
            });
            if (!text)
               item.setText("View file as chart");
            pop.add(item);

            item = new JMenuItem("Start QUICKVIEW.SH");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    String fileName = ((BIGOutputFile) selectedNode.getUserObject()).
                                      getFile().getAbsolutePath();
                    try {
                        Process p = Runtime.getRuntime().exec(bigInterface.
                                getBenchItPath() + File.separator + "tools" +
                                File.separator +
                                "QUICKVIEW.SH " + fileName);
                        bigInterface.getConsole().addStream(p.
                                getErrorStream(), BIGConsole.ERROR);
                        bigInterface.getConsole().addStream(p.
                                getInputStream(), BIGConsole.DEBUG);

                    } catch (IOException ex) {
                    }
                }
            });
            pop.add(item);
            pop.addSeparator();

            item = new JMenuItem( "Fix invalid values" ) ;
            item.addActionListener( new ActionListener()
            {
               public void actionPerformed( ActionEvent ae )
               {
                  // instance for cleaning bit files
                  system.BIGUtility util = new system.BIGUtility() ;
                  if ( BIGResultTree.this.gui.getStatusProgress() != null )
                  {
                     util.getObservable().addObserver(
                         BIGResultTree.this.gui.getStatusProgress() ) ;
                  }
                  String fileName = ( ( BIGOutputFile ) selectedNode.
                                      getUserObject() ).
                      getFile().getAbsolutePath() ;
                  // clean the file
                  Thread t = util.removeInfinitiesFromFile( fileName ) ;
               }
            } ) ;
            pop.add( item ) ;

            item = new JMenuItem( "Change architecture/display infos" ) ;
            item.addActionListener( new ActionListener()
            {
               public void actionPerformed( ActionEvent ae )
               {
                  TreePath[] paths = BIGResultTree.this.getSelectionPaths() ;
                  String[] availFiles =
                      ( new File( BIGInterface.getInstance().getBenchItPath() +
                                  File.separator + "LOCALDEFS" ) ).
                      list( new FilenameFilter()
                  {
                     public boolean accept( File f , String name )
                     {
                        if ( ( new File( f.getAbsolutePath() +
                                         File.separator +
                                         name + "_input_architecture" ) ).
                             exists() )
                        {
                           if ( ( new File( f.getAbsolutePath() +
                                            File.separator +
                                            name + "_input_display" ) ).
                                exists() )
                           {
                              return true ;
                           }
                        }
                        return false ;
                     }
                  } ) ;
                  JList jl = new JList( availFiles ) ;
                  jl.setPreferredSize( new Dimension( 300 , 450 ) ) ;
                  int select = JOptionPane.showConfirmDialog( gui , jl ,
                      "Select the architecture file" ,
                      JOptionPane.OK_CANCEL_OPTION ,
                      JOptionPane.QUESTION_MESSAGE ) ;
                  if ( select != JOptionPane.OK_OPTION )
                  {
                     return ;
                  }
                  if ( jl.getSelectedValue() == null )
                     return ;
                  for ( int i = 0 ; i < paths.length ; i++ )
                  {
                     DefaultMutableTreeNode actualNode = ( ( DefaultMutableTreeNode ) paths[ i ].
                                      getLastPathComponent() ) ;
                     String fileName = ( ( BIGOutputFile ) actualNode.
                                         getUserObject() ).
                         getFile().getAbsolutePath() ;
                     // get file content
                     File outputfile = null ;
                     try
                     {
                        outputfile = new File( fileName ) ;
                     }
                     catch ( Exception ex )
                     {
                        System.out.println( "There's no file selected" ) ;
                        return ;
                     }
                     String content = null ;
                     try
                     {
                        content = BIGFileHelper.getFileContent( outputfile ) ;
                     }
                     catch ( Exception ex1 )
                     {
                        System.out.println( "Could not load " + outputfile ) ;
                        return ;
                     }
                     // will contain everything before the architecture
                     String firstPart = "" ;
                     // will contain everything after the architecture
                     String lastPart = "" ;
                     // everything before hostname
                     try
                     {
                        firstPart = firstPart +
                            content.substring( 0 ,
                                               content.indexOf( "\nhostname=\"" ) +
                                               1 ) ;
                        // find hostname in resultfile and go to the character after the
                        // linebreak to hostname
                        content = content.substring( content.indexOf(
                            "\nhostname=\"" ) + 1 ) ;
                        firstPart = firstPart +
                            content.substring( 0 , content.indexOf(
                                "\n" ) + 1 ) ;
                        content = content.substring( content.indexOf(
                            "\n" ) + 1 ) ;
                        firstPart = firstPart +
                            content.substring( 0 , content.indexOf(
                                "\n" ) + 1 ) ;
                        content = content.substring( content.indexOf(
                            "\n" ) + 1 ) ;


                        // goto the line after hostname
                        // find end
                        content = content.substring( content.indexOf(
                            "\nbeginofenvironmentvariables\n" ) ) ;
                        lastPart = content ;
                     }
                     catch ( Exception ex2 )
                     {
                        ex2.printStackTrace();
                        System.out.println( "Error while parsing outputfile " +
                                            outputfile ) ;
                        return ;
                     }
                     // content will now be the architectur info
                     try
                     {
                        content = BIGFileHelper.getFileContent( new File(
                            BIGInterface.getInstance().getBenchItPath() +
                            File.separator + "LOCALDEFS" +
                            File.separator + jl.getSelectedValue() +
                            "_input_architecture" ) ) ;
                     }
                     catch ( Exception ex3 )
                     {
                        System.out.println( "Error while loading " + jl.
                                            getSelectedValue() ) ;
                        return ;
                     }
                     // now the input_display
                     //search for last fontsizelegendfunction
                     firstPart = firstPart + content ;
                     int index = 0 ;
                     int oldIndex = 0 ;
                     while ( true )
                     {
                        index = lastPart.indexOf( "\nfontsizelegendfunction" ,
                                                  oldIndex ) + 1 ;
                        if ( index > 0 )
                           oldIndex = index ;
                        // 0 because +1 two lines above
                        else
                        {
                           index = lastPart.indexOf( "\n" , oldIndex ) + 1 ;
                           firstPart = firstPart +
                               lastPart.substring( 0 , index ) ;
                           lastPart = lastPart.substring( index ) ;
                           break ;
                        }
                     }
                     // goto data (which is after display infos)
                     lastPart = lastPart.substring( lastPart.indexOf(
                         "beginofdata" ) ) ;
                     // content will now be the display info
                     try
                     {
                        content = BIGFileHelper.getFileContent( new File(
                            BIGInterface.getInstance().getBenchItPath() +
                            File.separator + "LOCALDEFS" +
                            File.separator + jl.getSelectedValue() +
                            "_input_display" ) ) ;
                     }
                     catch ( Exception ex3 )
                     {
                        System.out.println( "Error while loading " + jl.
                                            getSelectedValue() ) ;
                        return ;
                     }
                     // build all together
                     content = firstPart + content + lastPart ;
                     // save
                     try
                     {
                        BIGFileHelper.saveToFile( content , outputfile ) ;
                     }
                     catch ( Exception ex4 )
                     {
                        System.out.println( "Error while saving " + outputfile ) ;
                        return ;
                     }
                     System.out.println(
                         "Succesfully changed architecture/display infos for "+outputfile.getName() ) ;
                  }
               }
            } ) ;
            pop.add( item ) ;
            pop.addSeparator() ;

            item = new JMenuItem( "Delete" ) ;
            item.addActionListener( new ActionListener()
            {
               public void actionPerformed( ActionEvent ae )
               {

                  delete() ;
               }

            } ) ;
            pop.add( item ) ;
            pop.addSeparator() ;
            pop.add(sortMenu);
            return pop;
         } else {
             if (selectedNode.getUserObject() instanceof String
                 && (!(selectedNode.getParent()==null))&&(!
                 (( ( ( DefaultMutableTreeNode ) (
                     selectedNode.getParent() ) ).getUserObject() ) instanceof
                 plot.BIGResultMixer)
                 ))
             {
                 final JPopupMenu pop =  new JPopupMenu();
                 pop.add(sortMenu);
                 return pop;

             } else {

                JPopupMenu pop =  new JPopupMenu();
                if ((selectedNode.getUserObject() instanceof plot.
                     BIGResultMixer) ||
                    ( (!(selectedNode.getParent()==null))&&(
                 (( ( ( DefaultMutableTreeNode ) (
                     selectedNode.getParent() ) ).getUserObject() ) instanceof
                 plot.BIGResultMixer)
                 ))
) {
                    if ((selectedNode.getUserObject() instanceof plot.
                         BIGResultMixer))
                        pop = ((plot.BIGResultMixer) (selectedNode.
                                getUserObject())).
                              getBIGPopupMenu(
                                      BIGResultTree.this.gui,
                                      selectedNode.getUserObject());
                    else {
                        pop = ((plot.BIGResultMixer) (((DefaultMutableTreeNode) (
                                selectedNode.getParent())).getUserObject())).
                              getBIGPopupMenu(
                                      BIGResultTree.this.gui,
                                      selectedNode.getUserObject());

                    }

                    pop.addSeparator();
                    pop.add(sortMenu);
                    return pop;
                }
            }
        }
        return null;
     }
     public String getToolTipText(MouseEvent evt)
     {
        return this.computeToolTipText(evt);
     }
    public String computeToolTipText(MouseEvent evt)
    {
           final TreePath path = this.getClosestPathForLocation(evt.
               getX(), evt.getY());
           final DefaultMutableTreeNode node = ((DefaultMutableTreeNode) path.
                                              getLastPathComponent());

            // set tool tip text
            Object o = node.getUserObject() ;
            if ( o instanceof BIGOutputFile )
            {

                return o.toString()+" "+((BIGOutputFile)o).getComment();
            }
            else

            if (node.isLeaf()) {
                // a leaf tool tip is needed
                String tip = o.toString();
                this.setToolTipText(tip);
            } else if ((!node.equals(node.getRoot())) &&
                       (!node.isLeaf())) {
                // a kernel base name tool tip is needed
                int count = 0;
                //  for the BRM
                if (node.getUserObject() instanceof plot.BIGResultMixer) {
                    count = ((plot.BIGResultMixer) node.getUserObject()).
                            getNumberOfFunctions();

                } else {
                    count = getNumberOfSubOutputFiles(node);
                }
                String tip = "There ";
                if (count == 1) {
                    tip = tip + "is 1 result ";
                } else {
                    tip = tip + "are " + count + " results ";
                }
                tip = tip + "in this node.";
                return tip;
            }
            return "";
         }
         private plot.BIGOnlineMixer online = null ;
         public void setOnline( boolean on )
         {
            if ( ( this.online == null ) && on )
            {
               addOnlineMixer() ;
            }
            if ( ( this.online != null ) && !on )
            {
               removeOnlineMixer() ;
            }
         }
   public boolean isOnline()
   {
      if (online==null)
         return false;
      return true;
   }
   public plot.BIGOnlineMixer getOnlineMixer()
   {
      return online;
   }



   /**
    * removeOnlineMixer
    */
   private void removeOnlineMixer()
   {
      DefaultTreeModel model = ( ( DefaultTreeModel ) getModel() ) ;
      DefaultMutableTreeNode rootNode =
          ( DefaultMutableTreeNode ) ( model.getRoot() ) ;
      for (int i=0;i<rootNode.getChildCount();i++)
         if (rootNode.getChildAt(i) instanceof plot.BIGOnlineMixer)
            rootNode.remove(i);
      this.updateResultTree(rootNode);

   }
   private void addOnlineMixer()
   {
      DefaultTreeModel model = ( ( DefaultTreeModel ) getModel() ) ;
      DefaultMutableTreeNode rootNode =
          ( DefaultMutableTreeNode ) ( model.getRoot() ) ;
      this.online = new plot.BIGOnlineMixer( this.gui.getViewPanel() ,
                                             this.gui.getTextPanel() ,
                                             "Online-Mixer" ) ;
      this.online.setResultTree(this);
      rootNode.add(this.online.getTreeNode()) ;
      this.updateResultTree(rootNode);

   }
        /*
        public String getToolTipText()
        {
           System.err.println("3");
           return "test";
        }*/
}
/******************************************************************************
 *  Log-History
 *
 *  $Log: BIGResultTree.java,v $
 *  Revision 1.27  2007/07/10 12:25:52  tschuet
 *  it is possible to cancel the save-dialog of the zip feature
 *
 *  Revision 1.26  2007/07/10 11:39:26  tschuet
 *  zip feature use the last path as default
 *
 *  Revision 1.25  2007/07/03 11:29:14  tschuet
 *  correct actualizing of plot titles
 *
 *  Revision 1.24  2007/06/05 10:17:27  tschuet
 *  changes on handling the resulttree-popupmenu
 *
 *  Revision 1.23  2007/05/22 13:13:19  tschuet
 *  addition of an attempt for context menus on mac-systems
 *
 *  Revision 1.22  2007/05/08 09:43:30  tschuet
 *  reorganize imports
 *
 *  Revision 1.21  2007/05/08 09:31:05  tschuet
 *  in the context menu of BIGOutputFiles a new function to pack selelcted files in a ZIP-file was added
 *
 *  Revision 1.20  2007/04/10 10:11:14  tschuet
 *  if you move a graph from a mixer to another mixer (drag'n'drop) a copy of this graph will be produced so that changes only affect the actual graph
 *
 *  Revision 1.19  2007/02/27 12:37:49  tschuet
 *  different bugfixes (mixer, remote folder -> see meeting at february, 27th)
 *
 *  Revision 1.18  2006/12/07 14:54:22  rschoene
 *  removed bug in new functionality change architecture of result
 *
 *  Revision 1.17  2006/12/07 14:26:00  rschoene
 *  change architecture
 *
 *  Revision 1.16  2006/11/20 14:15:34  rschoene
 *  Drag n drop is what its meant to be like
 *
 *  Revision 1.15  2006/08/04 09:32:07  rschoene
 *  some updates and debugs
 *
 *  Revision 1.14  2006/07/05 09:28:38  rschoene
 *  debugged reset all plot-information
 *
 *  Revision 1.13  2006/07/04 08:30:26  rschoene
 *  debugged update
 *
 *  Revision 1.12  2006/07/01 10:47:14  rschoene
 *  again... removed bug
 *
 *  Revision 1.11  2006/07/01 10:28:40  rschoene
 *  removed bug in update()
 *
 *  Revision 1.10  2006/06/28 11:39:05  rschoene
 *  removed bug
 *
 *  Revision 1.9  2006/06/26 10:47:54  rschoene
 *  integrated online- in result-mode
 *
 *  Revision 1.8  2006/05/11 10:30:45  rschoene
 *  changes for reset plot-info, save height/width for save/change default colors
 *
 *  Revision 1.7  2006/02/07 16:36:40  rschoene
 *  all new plotting
 *
 *  Revision 1.6  2006/01/31 21:11:33  rschoene
 *  shows comment as tooltip
 *
 *  Revision 1.5  2006/01/09 16:35:23  rschoene
 *  some advances / debugging for saveAndLadPlotting
 *
 *  Revision 1.4  2005/12/07 10:24:34  rschoene
 *  ask once for deleting
 *
 *  Revision 1.3  2005/11/02 14:15:15  rschoene
 *  removed bugs and added multiple mixer support
 *
 *  Revision 1.2  2005/10/21 11:25:01  rschoene
 *  removed fills in RemoteMenu and delete-bug in ResultTree
 *
 *  Revision 1.1  2005/10/20 13:13:57  rschoene
 *  src3 add
 *
 *  Revision 1.5  2005/04/26 12:33:49  wloch
 *  fixed disapearance of certain results when changeing result sorting
 *
 *  Revision 1.4  2005/04/19 15:22:19  wloch
 *  implemented sorting in result tree
 *
 *  Revision 1.3  2005/02/22 17:01:54  wloch
 *  added subnode to BRM and instant loading
 *
 *  Revision 1.2  2005/02/21 19:13:16  wloch
 *  major changes to menu structure and popup menus
 *
 *  Revision 1.1  2005/02/18 09:45:19  wloch
 *  implemented a better popup menu API
 *
 *
 ******************************************************************************/

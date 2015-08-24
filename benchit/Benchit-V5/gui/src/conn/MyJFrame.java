package conn;

import javax.swing.*;
import java.awt.event.*;
import java.util.*;

/**
 * <p>Überschrift: MyJFrame</p>
 * <p>Beschreibung: a Frame that works with the conn-package-stuff.
 *  Once it's been independed to BIGGUI. now it isnt any longer</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */
public class MyJFrame
{
    /**
     * login panel
     */
    public MajorJPanel major;
    /**
     * contains graphMenu_close
     */
    private JMenu connectMenu;
    /**
     *logout
     */
    private JMenuItem graphMenu_close;
   /**
    * Creates a JFrame, that contains a MajorJPanel, that can connect to a server
    * @param ip the ip of the server
    */
   public MyJFrame(String ip, gui.BIGGUI gui)
   {
      // build menu
      // once, it was a separate one
      connectMenu= new JMenu("Database");
      connectMenu.setMnemonic(KeyEvent.VK_D);
      connectMenu.setEnabled(false);
      // now only the item is still needed
      graphMenu_close = new JMenuItem("Close Connection");
      graphMenu_close.addActionListener(
         new ActionListener()
         {
            public void actionPerformed(ActionEvent e)
            {
               try
               {
                  major.closeConnection();
               }
               catch (Exception evt)
               {
                  System.err.println(evt);
               }
            }
         }
      );
      connectMenu.add(graphMenu_close);
      major=new MajorJPanel(ip,connectMenu,gui);
   }
   /**
    * Returns JMenuItems in ordered Lists according to their category
    * for BenchIT database access.<br>
    * The returned array is of size 3 where the three indices represent the
    * following categories: [0] setup, [1] measure, [2] evaluate.<br>
    * Besides JMenuItems the Lists may also contain String objects with the
    * value "Separator". Any menu generating method should add a separator
    * to the menu at that place.
    * @return an array of 3 Lists of JMenuItems.
    **/
   public java.util.List[] getDBMenus() {
      java.util.List[] retval = new java.util.List[3];
      retval[0] = new ArrayList();
      retval[1] = new ArrayList();
      retval[2] = new ArrayList();
      // add menu items for category: setup
      // add menu items for category: measure
      // add menu items for category: evaluate
      retval[2].add( graphMenu_close );
      return retval;
   }
   /**
    * gets the Menu of this
    * @return the Menu of this
    */
   public JMenu getMenu() {
      return connectMenu;
   }
   /**
    * gets the DisplayPanel of this
    * @return this' displayPanel
    */
   public JComponent getDisplayPanel() {
      return major;
   }
   /**
    * sets the console to this mode (can only be shown once, so switch
    * when change BIG to another mode)
    * @param biggui BIGGUI reference to the BIGGUI
    */
   public void setConsole(gui.BIGGUI biggui)
   {
      this.major.my.setConsole(biggui);
   }
}

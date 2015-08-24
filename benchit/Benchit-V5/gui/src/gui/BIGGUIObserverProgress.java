/*********************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGGUIObserverProgress.java
*
*  Author: Robert Wloch
*  Last change by: $Author: tschuet $
*  $Revision: 1.2 $
*  $Date: 2007/05/08 09:41:53 $
*
*********************************************************************/
package gui;

import java.awt.Dimension;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JProgressBar;

import system.BIGInterface;

/**
  * This class is the progress bar located in the status bar.
  *
  * @see BIGGUI
  * @author Robert Wloch
  *    <a href="mailto:wloch@zhr.tu-dresden.de">wloch@zhr.tu-dresden.de</a>
  */
public class BIGGUIObserverProgress
   extends JProgressBar implements Observer
{
   // debug
   private BIGInterface bigInterface = BIGInterface.getInstance();
   private int debug = bigInterface.getDebug( "BIGGUI" );
   private BIGGUIProgressObservable observerParent = null;
   public BIGGUIObserverProgress( int min, int max )
   {
      super( min, max );
      setBorderPainted( true );
      setStringPainted( true );
      setString( "Progress:" );
      setValue( min );
   }
   /** This updates the value of the progress bar.
   * The value must be an <code>Integer</code> Object
   *
   * @param obs   the observable
   * @param value the new value for the progress bar
   */
   public void update( Observable obs, Object value )
   {
      if ( debug > 0 )
         System.out.println( "ProgressBar update is called "
            + value );
      int val = ((Integer)value).intValue();
      if ( !isStringPainted() ) setStringPainted( true );
      if ( val >= 100 )
      {
         setValue( getMinimum() );
         setStringPainted( false );
         if ( observerParent != null )
         {
            observerParent.progressFinished();
            observerParent = null;
         }
      }
      else
      {
         setString( val + " %" );
         setValue( val );
      }
   }
   /** Sets the BIGGUIProgressObservable interface.
     * The BIGGUIProgressObservable interface requieres the
     * method <code>progressFinished()</code> to be implemented.
     * Once the BIGGUIObserverProgress reaches it's maximum value it
     * will reset itself automatically and check if a
     * BIGGUIProgressObservable is waiting for a call back.<br>
     * You can use this for example to
     * <code>setVisible( false )</code> GUI components during the
     * progress bar updates and then have them set visible again
     * when the progress is done. This is especially neccessary if
     * you're using threads to do the progress.<br>
     * Also this method won't return until the value of the progress
     * bar is greater than <code>getMinimum()</code>. This ensures,
     * that the progress bar can't be used by more than one
     * progresses.
     * @param observable the BIGGUIProgressObservable
     */
   public synchronized void setProgressObservable(
      BIGGUIProgressObservable observable )
   {
      while ( observerParent != null )
      {
         try
         {
            wait( 100 );
         }
         catch ( InterruptedException ie ) {}
      }
      observerParent = observable;
   }
   public void start()
   {
      setValue( getMinimum() );
      setVisible( true );
   }
   /** Sets the progress bar to new limits. */
   public void start( int min, int max )
   {
      setMinimum( min );
      setMaximum( max );
      setPreferredSize( new Dimension(
         getMaximum(), 15 ) );
      setMinimumSize( new Dimension(
         getMaximum(), 15 ) );
      setMaximumSize( new Dimension(
         getMaximum(), 15 ) );
      setValue( getMinimum() );
      setVisible( true );
   }
}

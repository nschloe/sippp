/*********************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGGUIProgressObservable.java
*
*  Author: Robert Wloch
*  Last change by: $Author: rschoene $
*  $Revision: 1.1 $
*  $Date: 2005/10/20 13:13:56 $
*
*********************************************************************/
package gui;

/**
  * The BIGGUIProgressObservable interface requieres the
  * method <code>progressFinished()</code> to be implemented.
  * Once a BIGGUIObserverProgress reaches it's maximum value it
  * will reset itself automatically and check if a
  * BIGGUIProgressObservable is waiting for a call back.<br>
  * You can use this for example to
  * <code>setVisible( false )</code> GUI components during the
  * progress bar updates and then have them set visible again
  * when the progress is done. This is especially neccessary if
  * you're using threads to do the progress.<br>
  */
public interface BIGGUIProgressObservable
{
   public void progressFinished();
}

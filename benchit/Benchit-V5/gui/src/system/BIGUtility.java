package system;

/**
 * <p>Überschrift: </p>
 * <p>Beschreibung: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: </p>
 * @author unbekannt
 * @version 1.0
 */
import java.io.*;
import gui.BIGConsole;
import java.util.Vector;
import java.util.Observable;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JComponent;


public class BIGUtility {

    public String[] commands = new String[3];
    public Process p;
    private BIGObservable progress = new BIGObservable();

    public BIGUtility() {
    }

    public static void addComponent(
 	       JComponent container , JComponent component ,
 	       GridBagLayout gridbag , GridBagConstraints c )
    {
 	      gridbag.setConstraints( component , c ) ;
 	      container.add( component ) ;
    }
    
    public static void setConstraints( GridBagConstraints c ,
            int x , int y , int gw , int gh )
    {
 	   c.gridx = x ;
 	   c.gridy = y ;
 	   c.gridwidth = gw ;
 	   c.gridheight = gh ;
 	}
    
    public Thread removeInfinitiesFromFile(final String fileName) {
        final BIGObservable obs = (BIGObservable)this.getObservable();
        Thread t1 = new Thread() {
            public void run() {
                String text = "Removing Infinity from File:" + fileName;
                obs.setProgress(0);

                BufferedWriter outputStream = null;
                BufferedReader inputStream = null;
                // first we got to read the file
                try {
                    inputStream = new BufferedReader(new FileReader(
                            fileName));
                }
                // Any errors?
                catch (IOException exception) {
                    BIGInterface.getInstance().getConsole().postMessage(
                            "Couldn't open read-stream! :"
                            , BIGConsole.ERROR);
                    obs.setProgress(100);
                    return;
                }
                Vector v = new Vector();
                try {
                    do {
                        v.add(inputStream.readLine());
                    } while (!((String) (v.lastElement())).equals("endofdata"));
                    inputStream.close();
                } catch (Exception ex) {
                }
                String[] fileContent = new String[v.size()];
                for (int i = 0; i < fileContent.length; i++) {
                    fileContent[i] = (String) v.elementAt(i);
                }
                // read file
                obs.setProgress((int) (100 / 7));
                // Now we have the whole file in String[] fileContent and can calculate
                for (int i = 0; i < fileContent.length; i++) {
                    if (fileContent[i].indexOf("youtmax") != -1) {
                        if ((fileContent[i].substring(fileContent[i].indexOf(
                                "=") +
                                1,
                                fileContent[i].length()).
                             equals("inf"))
                            ||
                            (fileContent[i].substring(fileContent[i].indexOf(
                                    "=") +
                                1,
                                fileContent[i].length()).
                             equals("1E25"))
                                ) {
                            // this is what happens, if there is an inf(inity) or 1E25
                            int beginOfData = 0;
                            for (int j = 0; j < fileContent.length; j++) {
                                if (fileContent[j].equals("beginofdata")) {
                                    beginOfData = j + 1;
                                }
                            }
                            // now we save all data in an array
                            // getting number of columns
                            String workString = new String(new StringBuffer(
                                    fileContent[beginOfData]));
                            int countColumns = 0;
                            do {
                                workString = workString.substring(workString.
                                        indexOf(
                                                "\t") + 1, workString.length());
                                countColumns++;
                            } while (workString.indexOf("\t") != -1);
                            obs.setProgress((int) (200 / 7));

                            double[][] dataArray = new double[fileContent.
                                    length -
                                    beginOfData][countColumns];
                            // fill dataArray
                            for (int j = beginOfData;
                                         j < fileContent.length - 1;
                                         j++) {
                                workString = new String(new StringBuffer(
                                        fileContent[
                                        j]));
                                for (int k = 0; k < countColumns - 1; k++) {
                                    try {
                                        dataArray[
                                                j -
                                                beginOfData][k] = (new Double(
                                                workString.substring(0,
                                                workString.indexOf("\t")))).
                                                doubleValue();
                                    } catch (NumberFormatException ex1) {
                                        dataArray[
                                                j - beginOfData][k] = 1E25;
                                    }
                                    workString = workString.substring(
                                            workString.indexOf("\t") +
                                            1,
                                            workString.length());

                                    try {
                                        dataArray[
                                                j - beginOfData][countColumns -
                                                1] = (new Double(
                                                workString.substring(0,
                                                workString.length()))).
                                                doubleValue();
                                    } catch (NumberFormatException ex1) {
                                        dataArray[
                                                j - beginOfData][countColumns -
                                                1] = 1E25;
                                    }

                                }
                            }
                            obs.setProgress((int) (300 / 7));
                            // changing content in dataArray
                            for (int j = fileContent.length - 1;
                                         j >= beginOfData;
                                         j--) {
                                for (int k = countColumns - 1; k >= 0; k--) {
                                    if (dataArray[j - beginOfData][k] == 1E25) {
                                        dataArray[j - beginOfData][k] = -1;
                                        /* if ( (j - beginOfData) == 0) {
                                          dataArray[0][k] = -1dataArray[1][k];
                                         }
                                         else {
                                         if ( (dataArray[j - beginOfData - 1][k]) ==
                                              1E25) {
                                         dataArray[j - beginOfData][k] = dataArray[j -
                                                beginOfData + 1][k];
                                          }
                                          else {
                                            dataArray[j -
                                         beginOfData][k] = (dataArray[j -
                                                beginOfData +
                                         1][k] + dataArray[j - beginOfData -
                                                1][k]) /
                                                2;
                                          }
                                         }*/
                                    }
                                }
                            }
                            // getting max y
                            obs.setProgress((int) (400 / 7));
                            double maxY = -1;
                            for (int j = fileContent.length - 1;
                                         j >= beginOfData;
                                         j--) {
                                for (int k = countColumns - 1; k >= 0; k--) {
                                    if ((dataArray[j - beginOfData][k]) > maxY) {
                                        maxY = dataArray[j - beginOfData][k];
                                    }
                                }
                            }
                            obs.setProgress((int) (500 / 7));
                            try {
                                outputStream = new BufferedWriter(new
                                        FileWriter(
                                                BIGInterface.getInstance().
                                                getBenchItPath()
                                                + java.io.File.separator +
                                                "output"
                                                + java.io.File.separator +
                                                fileName));
                            } catch (IOException ex2) {
                                BIGInterface.getInstance().getConsole().
                                        postMessage(
                                                "Couldn't open write-stream!",
                                                BIGConsole.ERROR);
                            }
                            int lineToFile = 0;
                            StringBuffer outSB = new StringBuffer();
                            while ((fileContent[lineToFile]).indexOf("youtmin") ==
                                   -1) {
                                outSB.append(fileContent[lineToFile] + "\n");
                                lineToFile++;
                            }
                            outSB.append("youtmin=0\n");
                            lineToFile++;

                            obs.setProgress((int) (600 / 7));
                            while ((fileContent[lineToFile]).indexOf("youtmax") ==
                                   -1) {
                                outSB.append(fileContent[lineToFile] + "\n");
                                lineToFile++;
                            }
                            outSB.append("youtmax=" + maxY + "\n");
                            do {
                                lineToFile++;
                                outSB.append(fileContent[lineToFile] + "\n");
                            } while (fileContent[lineToFile].indexOf(
                                    "beginofdata") ==
                                     -1);
                            for (int j = 0; j < dataArray.length; j++) {
                                String toWrite = new String();
                                for (int k = 0; k < dataArray[0].length; k++) {
                                    toWrite = toWrite + dataArray[j][k] + "\t";
                                }
                                outSB.append(toWrite + "\n");
                            }
                            try {
                                outSB.append("endofdata\n");
                                outputStream.write(outSB.toString());
                                outputStream.close();
                            } catch (IOException ex4) {
                                BIGInterface.getInstance().getConsole().
                                        postMessage(
                                                "Error while writing!",
                                                BIGConsole.ERROR);
                            }
                        } else {
                            obs.setProgress(100);
                            return;
                        }
                        obs.setProgress(100);
                        break;
                    }
                }
                obs.setProgress(100);
            }
        };
        t1.start();
        return t1;
    }

    public static void saveToCsv(File file, double[][] values,
                                 String[] identifier,
                                 String[] additionalInformation) {
        try {
            file.createNewFile();
        } catch (IOException ioe) {
            System.err.println("Couldn't write file. No rights?");
        }

        StringBuffer strOutput = new StringBuffer();
        strOutput.append("BenchIT-generated csv\n");
        for (int i = 0; i < additionalInformation.length; i++) {
            strOutput.append(additionalInformation[i] + "\n");
        }
        for (int i = 0; i < identifier.length; i++) {
            strOutput.append(identifier[i] + ";");
        }
        strOutput.deleteCharAt(strOutput.length() - 1); // removing the last ;
        strOutput.append("\n");
        for (int i = 0; i < values[0].length; i++) {
            for (int j = 0; j < values.length; j++) {
                strOutput.append(values[j][i]);
                while (strOutput.lastIndexOf(".") > (strOutput.length() - 4)) {
                    strOutput.append(0);
                }
                strOutput.append(";");

            }
            strOutput.deleteCharAt(strOutput.length() - 1); // removing the last ;
            strOutput.append("\n");
        }
        try {
            FileWriter outputStream = new FileWriter(file);

            outputStream.write(strOutput.toString());
            outputStream.flush();
            outputStream.close();
        } catch (IOException exception) {
            System.err.println(
                    "Error while writing data to file! No rights?");
            return;
        }

    }
    public static String getContent(InputStream in)
    {
       int i=0;
       StringBuffer fileContent = new StringBuffer() ;
      byte[] buffer=new byte[1000];
      int readData = 0 ;
      try
      {

      do
      {
            readData = in.read( buffer ) ;
         if (readData>0)
            fileContent.append(new String(buffer,0,readData));
         if (readData>-1)
            System.err.println("i:"+(i++)+"/"+readData);
      }
      while ( true ) ;
      }
      catch ( IOException ioe )
      {
      String s=fileContent.toString();
      return s;

      }

    }

    class BIGObservable extends Observable {
        private int oldValue = -1;
        public void setProgress(int value) {
            if (oldValue != value) {
                oldValue = value;
                setChanged();
                notifyObservers(new Integer(value));
                try {
                    Thread.sleep(0, 1);
                } catch (InterruptedException ie) {}
            }
        }
    }


    public Observable getObservable() {
        return progress;
    }

    public static void setFonts(org.jfree.chart.axis.NumberAxis axis, String axisName)
    {
       BIGInterface interf = interf = system.BIGInterface.getInstance() ; ;
       try
       {
          String name = interf.getBIGConfigFileParser().stringCheckOut( axisName+"Font" ) ;
          int style = interf.getBIGConfigFileParser().intCheckOut(
              axisName+"FontStyle" ) ;
          int size = interf.getBIGConfigFileParser().intCheckOut( axisName+"FontSize" ) ;
          axis.setLabelFont( new Font( name , style , size ) ) ;
       }
       catch ( Exception ex1 )
       {
          System.out.println(axisName+"Font Specification not found in BGUI.cfg");
          System.out.println("Using default");
       }

       try
       {
          String tname = interf.getBIGConfigFileParser().stringCheckOut(
              axisName+"TickFont" ) ;
          int tstyle = interf.getBIGConfigFileParser().intCheckOut(
              axisName+"TickFontStyle" ) ;
          int tsize = interf.getBIGConfigFileParser().intCheckOut(
              axisName+"TickFontSize" ) ;
          axis.setTickLabelFont( new Font( tname , tstyle , tsize ) ) ;
       }
       catch ( Exception ex )
       {
          System.out.println(axisName+"TickFont Specification not found in BGUI.cfg");
          System.out.println("Using default");
       }

   }

   public static Font getFont(String name)
   {
      BIGInterface interf = interf = system.BIGInterface.getInstance() ; ;
      try
      {
         String fname = interf.getBIGConfigFileParser().stringCheckOut( name+"Font" ) ;
         int style = interf.getBIGConfigFileParser().intCheckOut(
             name+"FontStyle" ) ;
         int size = interf.getBIGConfigFileParser().intCheckOut( name+"FontSize" ) ;
         return new Font( fname , style , size ) ;
      }
      catch ( Exception ex1 )
      {
         Font f=new Font("Serif",0,10);
         System.out.println(name+"Font Specification not found in BGUI.cfg");
         System.out.println("Setting default (Serif,plain,10)");
         interf.getBIGConfigFileParser().save() ;
         File bguicfg = interf.getBIGConfigFileParser().getFile() ;
         String content = BIGFileHelper.getFileContent( bguicfg ) ;
         StringBuffer sb = new StringBuffer( content ) ;
         sb.append( "\n# start auto filled axis font settings\n" ) ;
         sb.append( name + "Font = " + f.getName() + "\n" ) ;
         sb.append( name + "FontStyle = " + f.getStyle() +
                    "\n" ) ;
         sb.append( name + "FontSize = " + f.getSize() +
                    "\n" ) ;
         sb.append( "# end auto filled axis font settings" ) ;
         BIGFileHelper.saveToFile( sb.toString() , bguicfg ) ;
         interf.setBIGConfigFileParser( new
                                        BIGConfigFileParser( bguicfg.
             getAbsolutePath() ) ) ;
         return f;
      }

   }

   public static Font getFont(String name,File file)
   {
      BIGConfigFileParser p=new BIGConfigFileParser(file.getAbsolutePath());
      try
      {
         String fname = p.stringCheckOut( name+"Font" ) ;
         int style = p.intCheckOut(
             name+"FontStyle" ) ;
         int size = p.intCheckOut( name+"FontSize" ) ;
         return new Font( fname , style , size ) ;
      }
      catch ( Exception ex1 )
      {
         return null;
      }

   }

}

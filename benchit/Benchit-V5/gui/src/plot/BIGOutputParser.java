/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGOutputParser.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: rschoene $
 *  $Revision: 1.10 $
 *  $Date: 2006/07/04 11:12:40 $
 *
 ******************************************************************************/
package plot;

import java.util.*;
import system.*;
import java.io.*;

/**
 * a parser for getting the information from the BenchIT outputfiles (*.bit)
 *
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @author <a href="mailto:pisi@pisi.de">Christoph Mueller</a>
 **/
public class BIGOutputParser {

    /**
     * debugmessages en- or disabled
     * (may be overwritten in the constructor)
     */
    private boolean debug = false;

    /**
     * holds the complete content of the file
     */
    private BIGStrings text;

    /**
     * the name of the file stored in text
     */
    //private String filename;

    /** The referrence to the Observer class for the progress bar. */
    private BIGObservable progress;

    /** The last loaded fileName*/
    private String fileName = null;
    /**
     * instantiate an OutputParser without initially loaded file
     *
     */
    public BIGOutputParser() {
        if (BIGInterface.getInstance().getDebug("BIGOutputParser") > 0) {
            debug = true;
        }
        progress = new BIGObservable();
        text = new BIGStrings();
        //filename = new String();
    }

    /**
     * instantiate an OutputParser with initially loaded file
     * @param filename the file to load
     * @throws BIGParserException there was an error accessing the file
     */
    public BIGOutputParser(String filename) throws BIGParserException {
        if (BIGInterface.getInstance().getDebug("BIGOutputParser") > 0) {
            debug = true;
        }
        progress = new BIGObservable();
        text = new BIGStrings();
        this.load(filename);
        this.fileContent=BIGFileHelper.getFileContent(new File(filename));
    }

    /**
     * load the content of a file into the internal BIGStrings text
     * @param filename the name of the file to load
     * @throws BIGParserException there was an error accessing the file
     */
    public void load(String filename) throws BIGParserException {
        this.fileName = filename;
        try {
            text.readFromFile(filename);
            if (debug) {
                System.out.println(
                        "BIGOutputParser: file [" + filename + "] loaded");
            }
            //this.filename = filename;
        } catch (Exception e) {
            throw new BIGParserException(
                    "BIGOutputParser: Error while instantiating an OutputParser:\n"
                    + "BIGOutputParser: could not read from file ["
                    + filename
                    + "]\n"
                    + "BIGOutputParser: cause: ["
                    + e.getMessage()
                    + "]");
        }
    }

    private BIGStrings open(String filename) throws BIGParserException {
        BIGStrings text = new BIGStrings();
        try {
            text.readFromFile(filename);
            if (debug) {
                System.out.println(
                        "BIGOutputParser: file [" + filename + "] loaded");
            }
            //this.filename = filename;
        } catch (Exception e) {
            throw new BIGParserException(
                    "BIGOutputParser: could not read from file ["
                    + filename
                    + "]\n"
                    + "BIGOutputParser: cause: ["
                    + e.getMessage()
                    + "]");
        }
        return text;
    }

    /**
     * parse the internal BIGStrings text for outputdata
     * @return array d with d[0][] are the x values and d[n][] are the corresponding y values for series n
     */
    public double[][] parseOutputToData() {
        return parseOutputToData(text);
    }

    /**
     * parse a given BIGStrings text for outputdata
     * @param text the text to parse
     * @return the parsed outputData (@see this.parseOutputData)
     */
    public double[][] parseOutputToData(BIGStrings text) {
        // set to data, that doesnt exist or is wrong
        double errorDou=-1.0;
        try
        {
           errorDou = 1.0 *
               system.BIGInterface.getInstance().getBIGConfigFileParser().
               intCheckOut( "errorInt" ) ;
        }
        catch ( Exception ignored )
        {
        }


        // whether there is an inf or a NaN
        boolean fileNeedsCleaning = false;
        // looking where is data
        int lineNr = 0;
        // variables for using a progress bar
        // numValues is the number of values per function
        int numValues = 0;
        // percentDone is the current value index
        int percentDone = 0;
        //for (int i=0;i<text.size();i++)
        //System.err.println(i+":"+text.get(i));
        // get end of datasection
        while ((lineNr < text.size())
               && (!text.get(lineNr).trim().equals(
                       "endofdata"))) {
            lineNr++;
        }
        if (lineNr>=text.size())
        {
           if (debug) {
               System.out.println("BIGOutputParser: no endofdata tag");
           }
           lineNr=lineNr-1;
           text.set(lineNr,"endofdata");
           BIGFileHelper.saveToFile(text.toString(),new File(fileName));
           System.out.println("Corrupt file, try to clean...");


        }
        if (!text.get(lineNr).equals("endofdata")) {
            if (debug) {
                System.out.println("BIGOutputParser: no endofdata tag");
            }
            return new double[0][];
        }

        if (debug) {
            System.out.println(
                    "BIGOutputParser: endofdata tag in line " + lineNr);
        }

        // set max lines number
        numValues = lineNr;

        lineNr = 0;
        //move to begin of datasection
        while ((lineNr < text.size())
               && (!text.get(lineNr).trim().equals(
                       "beginofdata"))) {
            lineNr++;
        }

        if (!text.get(lineNr).equals("beginofdata")) {
            if (debug) {
                System.out.println("BIGOutputParser: no "
                                   + "beginofdata tag");
            }
            return new double[0][];
        }

        // subtract begin of data tag
        numValues = numValues - lineNr - 1;

        if (debug) {
            System.out.println(
                    "BIGOutputParser: beginofdata tag in line " + lineNr);
        }

        // extracting data
        lineNr++;
        ArrayList tempResult = new ArrayList();
        while ((lineNr < text.size())
               && (!text.get(lineNr).trim().equals("endofdata"))) {
            // set new value of the progress bar
            percentDone++;
            progress.setProgress((50 * percentDone) / numValues);

            ArrayList lineResult = new ArrayList();
            String line = text.get(lineNr).trim();
            if (debug) {
                System.out.println(
                        "BIGOutputParser: line:" + lineNr + " [" + line + "]");
            }
            int linePos = 0;

            //adding all values followed by a "\t"
            while (line.indexOf("\t", linePos + 1) > 0) {
                try {
                    String entry =
                            line.substring(
                                    linePos,
                                    line.indexOf("\t", linePos + 1));
                    lineResult.add(entry);
                } catch (Exception e) {
                    System.err.println(
                            "BIGOutputParser: error on parsing line:"
                            + lineNr
                            + " ["
                            + line
                            + "]");
                    lineResult.add("");
                }
                linePos = line.indexOf("\t", linePos + 1);
            }

            //adding the last value of this line
            String entry = line.substring(linePos + 1);
            lineResult.add(entry);

            //adding the line to the list of lines
            tempResult.add(lineResult);
            lineNr++;
        }
        progress.setProgress(50);

        // converting data
        if ((tempResult.isEmpty())||(tempResult.get(0) == null)) {
            if (debug) {
                System.out.println("BIGOutputParser: no data found");
            }
            return new double[0][];
        }
        //double[][] result = new double[][5];
        double[][] result =
                new double[((ArrayList) tempResult.get(0))
                .size()][tempResult
                .size()];

        for (int spalte = 0; spalte < result.length; spalte++) {
            for (int zeile = 0; zeile < result[0].length; zeile++) {
                // set new value of the progress bar
//             [ ( z       )   (   ) ]   ( 1 )
//             [ ( - * 100 ) + ( s ) ] * ( - )
//             [ ( Z       )   (   ) ]   ( S )
                progress.setProgress(
                        (((50 * zeile + spalte) / result[0].length) *
                         (1 / result.length)) + 50);

                String entry =
                        (String) ((ArrayList) tempResult.get(zeile)).get(spalte);
                   if (debug) BIGInterface.getInstance().getConsole().
                                postMessage(entry,2);

                try {
                    result[spalte][zeile] = (new Double(
                            entry)).doubleValue();
                } catch (NumberFormatException ex) {
                   // if (entry.trim().compareTo("-") != 0) {
                        /*BIGInterface.getInstance().getConsole().
                                postMessage(
                                        "Found \"Infinity\" or \"NaN\" in file!",
                                        2);
                        BIGInterface.getInstance().getConsole().
                                postMessage(
                                        "Cleaning File and set it to errorInt",
                                        2);*/
                                         fileNeedsCleaning = true;
                    result[spalte][zeile] = errorDou;
                    }
               // }

                if (debug) {
                    System.out.println(
                            "BIGOutputParser: "
                            + spalte
                            + " \t"
                            + zeile
                            + " \t"
                            + result[spalte][zeile]);
                }
            }
        }
        progress.setProgress(100);
        if ((fileNeedsCleaning) && (fileName != null)) {
            (new system.BIGUtility()).removeInfinitiesFromFile(fileName);
        }
        return result;
    }


    public static double[][] parseOutputToData(File f)
    {
       return parseOutputToData(BIGFileHelper.getFileContent(f));
    }
    public static double[][] parseOutputToData(String s)
    {
      double errorDou=-1.0;
      try
      {
         errorDou = 1.0 *
             system.BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "errorInt" ) ;
      }catch ( Exception ignored ){}

       boolean debug=false;
        if (BIGInterface.getInstance().getDebug("BIGOutputParser") > 0) {
            debug = true;
        }

       double[][] returnDouble;
       String dataString = null ;
      try
      {
         dataString = s.substring( s.lastIndexOf( "beginofdata\n" ) + 12 ,
                                   s.lastIndexOf( "\nendofdata" ) ) ;
      }
      catch ( Exception ex )
      {
         System.err.println(
             "There's no beginofdata and/or endofdata in this file" ) ;
         return new double[0][0];

      }
       StringTokenizer dataTokenizer=new StringTokenizer(dataString,"\n");
       String dataLine;
       dataLine=dataTokenizer.nextToken();
       if (dataLine==null)
          return new double[0][0];
       StringTokenizer lineTokenizer = new StringTokenizer( dataLine , "\t" ) ;
       returnDouble = new double[lineTokenizer.countTokens() ][ dataTokenizer.
           countTokens()+1 ] ; // +1 because we already have 1 line
       if (debug)
       {
          System.err.println( "y-functions="+lineTokenizer.countTokens() ) ;
          System.err.println( "measurementpoints="+(dataTokenizer.countTokens()-1) ) ;
       }
       int i=0,j=0;
       while (dataLine!=null)
       {
          if (debug)
             System.err.println("parsing dataLine ("+i+"):"+dataLine);
          j=0;
          lineTokenizer = new StringTokenizer( dataLine , "\t" ) ;
          while (lineTokenizer.hasMoreTokens())
          {
             if (debug)
                System.err.println("j="+j+"\ti="+i);
            try
            {
               returnDouble[ j ][ i ] = Double.parseDouble( lineTokenizer.
                   nextToken() ) ;
            }
            catch ( NumberFormatException ex1 )
            {
               returnDouble[j][i]=errorDou;
            }
             j++ ;
          }
          if ( dataTokenizer.hasMoreElements() )
             dataLine = dataTokenizer.nextToken() ;
          else
             dataLine = null ;
          i++ ;
       }
       return returnDouble;
    }
    private String fileContent=null;

    public static String getValue(String s, String fileContent)
    {
       if (fileContent==null)
          return "";
       // if not found
       if (fileContent.indexOf(s)==-1) return "";
       int lastIndex=0;
       // while we found a not commented version of the string s
       while ( fileContent.indexOf( s, lastIndex ) -
               fileContent.substring( 0 , fileContent.indexOf( s, lastIndex ) ).
               lastIndexOf( "#" )
               <

               fileContent.indexOf( s, lastIndex ) -
               fileContent.substring( 0 , fileContent.indexOf( s, lastIndex ) ).
               lastIndexOf( "\n" )
           )
       {
          lastIndex=fileContent.indexOf( s, lastIndex )+1;
          if (fileContent.indexOf( s, lastIndex )==-1)
             return "";
       }
          lastIndex=fileContent.indexOf( s, lastIndex )+1;
       // found line
       String lineWithData=fileContent.substring(lastIndex-1);
       String set=lineWithData.substring(0,lineWithData.indexOf("=")).trim();
       if (!set.equals(s))
          return getValue(s,fileContent.substring(fileContent.indexOf("\n",lastIndex)));
       // returning everything after the '=' trimmed
       int endIndex=lineWithData.indexOf( "\n" );
       while (lineWithData.charAt(endIndex-1)=='\\')
       {
          endIndex = lineWithData.indexOf( "\n" , endIndex+1 ) ;
       }
       lineWithData = lineWithData.substring( lineWithData.indexOf( "=" ) + 1 ,
                                              endIndex );
       lineWithData=lineWithData.replaceAll("\\\n","");
       lineWithData=lineWithData.trim() ;
       if (lineWithData.startsWith("\"")&&lineWithData.endsWith("\""))
          lineWithData=lineWithData.substring(1,lineWithData.length()-1);
       return lineWithData;

    }

    public String getValue(String s)
    {
       // if not found
       if (fileContent.indexOf(s)==-1) return null;
       int lastIndex=0;
       // while we found a not commented version of the string s
       while ( fileContent.indexOf( s, lastIndex ) -
               fileContent.substring( 0 , fileContent.indexOf( s, lastIndex ) ).
               lastIndexOf( "#" )
               <

               fileContent.indexOf( s, lastIndex ) -
               fileContent.substring( 0 , fileContent.indexOf( s, lastIndex ) ).
               lastIndexOf( "\n" )
           )
       {
          lastIndex=fileContent.indexOf( s, lastIndex )+1;
          if (fileContent.indexOf( s, lastIndex )==-1)
             return null;
       }
          lastIndex=fileContent.indexOf( s, lastIndex )+1;
       // found line
       String lineWithData=fileContent.substring(lastIndex);
       // returning everything after the '=' trimmed
       lineWithData = lineWithData.substring( lineWithData.indexOf( "=" ) + 1 ,
                                              lineWithData.indexOf( "\n" ) );
       lineWithData=lineWithData.trim() ;
       if (lineWithData.startsWith("\"")&&lineWithData.endsWith("\""))
          lineWithData=lineWithData.substring(1,lineWithData.length()-1);
       if ( debug )
          System.out.println ("found "+s+" :"+lineWithData);
       return lineWithData;
    }


    /**
     * get the text of the loaded file
     * @return the text of the loaded file
     */
    public BIGStrings getAllText() {
        return text;
    }

    /**
     * get the text of a given file
     * @param filename the name of the file
     * @return the text of the file
     * @throws BIGParserException something was wrong while opening the file
     */
    public BIGStrings getAllText(String filename) throws BIGParserException {
        return open(filename);
    }

    /**
     * @deprecated
     * get the data from the inernally stored file
     * @return the parsed data
     */
    public double[][] getDataFromOutputFile() {
        return parseOutputToData();
    }

    /**
     * get the data from a given file
     * @param filename the name of the file to parse
     * @return the parsed data
     * @throws BIGParserException there was an error accessing the file
     */
    public double[][] getDataFromOutputFile(String filename) throws
            BIGParserException {
        return parseOutputToData(getAllText(filename));
    }

    /**
     * try to find values for given parameters
     * @param names strings representing names of variables
     * @return a Map keys are the names of the vars values are the values
     */
    public Map getParams(BIGStrings names) {
        Map result = new HashMap();

        //parse file for names and values
        String row, name, value;
        Iterator namesIt;
        Iterator textIt = text.iterator();

        // variables for using a progress bar
        int s = 0, z = 0;
        int sMax = text.size(), zMax = names.size();
        while (textIt.hasNext()) {
            s++;
            namesIt = names.iterator();
            row = (String) textIt.next();
            row = row.trim();
            if (row.indexOf("=") > 0) {
                while (namesIt.hasNext()) {
                    z++;
                    // set new value of the progress bar
//                [ ( z       )   (   ) ]   ( 1 )
//                [ ( - * 100 ) + ( s ) ] * ( - )
//                [ ( Z       )   (   ) ]   ( S )
                    progress.setProgress(
                            ((100 * z + s) / zMax) * (1 / sMax));

                    name = (String) namesIt.next();
                    // we must assure, that names like xyz1 don't
                    // interfere with names like xyz112
                    if (row.startsWith(name)) {
                        String varname = row.substring(0, row.indexOf("="));
                        if (varname.compareTo(name) == 0) {
                            value = row.substring(row.indexOf("=") + 1);
                            //remove eventually existing " at both ends
                            if (value.startsWith("\"")
                                && value.endsWith("\"")
                                && value.length() > 1) {
                                value =
                                        value.substring(
                                                value.indexOf("=") + 2,
                                                value.length() - 1);
                            }
                            if (debug) {
                                System.out.println(
                                        "BIGOutputParser: name \""
                                        + name
                                        + "\" found with value \""
                                        + value
                                        + "\"");
                            }
                            result.put(name, value);
                        }
                    }
                }
            }
        }
        progress.setProgress(100);
        return result;
    }

    /**
     * try to find values for given parameters
     * @param filename the nme of the file to load
     * @param names strings representing names of variables
     * @return a Map keys are the names of the vars values are the values
     * @throws BIGParserException there was an error accessing the file
     */
    public Map getParams(String filename, BIGStrings names) throws
            BIGParserException {
        Map result = new HashMap();

        //read file into BIGStrings
        BIGStrings text = open(filename);

        //parse file for names and values
        String row, name, value;
        Iterator namesIt;
        Iterator textIt = text.iterator();
        while (textIt.hasNext()) {
            namesIt = names.iterator();
            row = (String) textIt.next();
            row = row.trim();
            while (namesIt.hasNext()) {
                name = (String) namesIt.next();
                if (row.startsWith(name)) {
                    value = row.substring(row.indexOf("=") + 1);
                    //remove eventually existing " at both ends
                    if (value.startsWith("\"")
                        && value.endsWith("\"")
                        && value.length() > 1) {
                        value =
                                value.substring(
                                        value.indexOf("=") + 2,
                                        value.length() - 1);
                    }
                    if (debug) {
                        System.out.println(
                                "BIGOutputParser: value \""
                                + value
                                + "\" found for name \""
                                + name
                                + "\"");
                    }
                    result.put(name, value);
                }
            }
        }

        return result;
    }

    /** This Method is needed for adding this class Observable
     * to an Observer of a progress bar.
     * @return the progress of this (progress of an internal process)
     *  */
    public Observable getObservable() {
        return progress;
    }

    /** The internel class for the progress updates. */
    private class BIGObservable extends Observable {
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

}
/*****************************************************************************
 Log-History

 *****************************************************************************/

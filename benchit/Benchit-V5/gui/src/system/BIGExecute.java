/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGExecute.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: rschoene $
 *  $Revision: 1.7 $
 *  $Date: 2006/12/06 06:48:19 $
 *
 ******************************************************************************/
package system;

import java.io.*;
import java.util.*;

/**
 * this class was written to execute external programs such as shell scripts and to return their output
 *
 * @author <a href="mailto:pisi@pisi.de">Christoph Mueller</a>
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 *
 */
public class BIGExecute {
    private boolean debug = false;
    // my instance
    private static BIGExecute bigExecute;

    // the Interface instance
    private BIGInterface db;

    // is there a programm running or not?!?
    private boolean running = false;

    // list of ShellScripts to check for
    private static BIGStrings shellScriptList = null;

    // list of what kernels i can run?
    private Map kernelList;

    // set of all recognized source languages (needed for kernel tree sorting)
    private Set languages = null;

    /** The all indicator tag in the kernel list. */
    public static final String ALL_INDICATOR = "<all>";

    /** The script what is run by default on startKernel(name). */
    public static final String DEFAULT_SCRIPT = "SUBDIREXEC.SH";
    public static final String BOTH = "SUBDIREXEC.SH";
    public static final String COMPILE = "COMPILE.SH";
    public static final String RUN = "RUN.SH";

    /** Indicator for map sorting according to the real directory structure. */
    public static final int KERNEL_DIRECTORY = 0;

    /** Indicator for map sorting according to the source language of kernels and
     * then by the real directory structure. */
    public static final int SOURCE_LANGUAGE = 1;

    /**
     * The constructor.
     **/
    private BIGExecute() {
        db = BIGInterface.getInstance();
        if (db.getDebug("BIGExecute") > 0) {
            debug = true;
        }

        checkSourceLanguages();

        if (shellScriptList == null) {
            shellScriptList = new BIGStrings();
            shellScriptList.add("SUBDIREXEC.SH");
            shellScriptList.add("COMPILE.SH");
            shellScriptList.add("RUN.SH");
        }

/*        createAvailableKernelsFile( new File( BIGInterface.getInstance().
                                              getBenchItPath() + File.separator +
                                              "kernel" + File.separator ) ) ;*/
        kernelList = loadKernelList();
    }

    /** Forces BIGExecute to reload the list of recognized source languages.
     *
     * @deprecated
     *  */
    public void checkSourceLanguages() {
        languages = new HashSet();
        String sources = null;
        try {
            sources = db.getBIGConfigFileParser().stringCheckOut(
                    "sourceLanguages");
        } catch (Exception ex) {
            sources = null;
        }
        if (sources != null) {
            StringTokenizer st = new StringTokenizer(sources, "\", ", false);
            while (st.hasMoreTokens()) {
                languages.add(st.nextToken());
            }
        }
    }

    /**
     * Returns the one and only instance of the execute.
     * If there doesn't exist one, it creates one.
     *
     * @return the instance pointer
     **/
    public static BIGExecute getInstance() {
        if (bigExecute == null) {
            bigExecute = new BIGExecute();
        }
        return bigExecute;
    }

    /**
     * Executes an external program given by <code>cmd</code>
     * and waits until is has finished.
     *
     * @param cmd the command to be executed.
     * @return the return value of the external command.
     * @deprecated
     **/
    private int execute(String cmd) {
        return this.execute(cmd, true);
    }

    /**
     * Executes an external program given by <code>cmd</code>
     * and waits until is has finished.
     *
     * @param waitfor true if we want to wait until command has finished.
     * @param cmd the command to be executed.
     * @return the return value of the external command.
     * @deprecated
     **/
    private int execute(String cmd, boolean waitfor) {
        running = true;
        Process p = null;
        if (debug) {
            System.out.println("BIGExecute: executing:" + cmd);
        }
        // starting
        try {
            p = Runtime.getRuntime().exec(cmd);
        } catch (IOException e) {
            System.err.println("BIGExecute: failed to execute: "
                               + cmd
                               + "\n"
                               + e.getMessage());
            e.printStackTrace();
         }
          BIGInterface.getInstance().getConsole().addStream( p.getInputStream() ,
              gui.BIGConsole.DEBUG ) ;
          BIGInterface.getInstance().getConsole().addStream( p.getErrorStream() ,
              gui.BIGConsole.WARNING ) ;

        if (debug) {
            System.out.println(
                    "BIGExecute: command started - now waiting");
        }
        // waiting for finishing
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            System.err.println(
                    "BIGExecute: waiting for started process has been interrupted");
            System.err.println(e.getMessage());
            e.printStackTrace();
         }
        if (debug) {
            System.out.println(
                    "BIGExecute: command \"" + cmd + "\" finished");
        }
        running = false;
        return p.exitValue();
    }
    /**
     * @deprecated
     * @param path String
     * @return Map
     */
    public static Map checkShellScriptExistance(String path) {
        Map result = new HashMap();
        Iterator it = shellScriptList.iterator();
        while (it.hasNext()) {
            String name = (String) it.next();
            File file = new File(path + File.separator + name);
            if (file.exists()) {
                result.put(name, file.getAbsolutePath());
            }
        }
        return result;
    }
    /**
     * @deprecated
     */
    public void reloadKernelList() {
        String filename = db.getBenchItPath() + File.separator
                          + "gui" + File.separator + "cfg" + File.separator
                          + "availablekernels.txt";
        File f = new File(filename);
        if (f.exists()) {
            f.delete();
        }
//        createAvailableKernelsFile(new File(BIGInterface.getInstance().getBenchItPath()+File.separator+"kernel"+File.separator));
        kernelList = loadKernelList();
    }

    /**
     * Loads the availablekernles.txt file from the GUI's
     * <code>cfg</code> folder and scans those folders for
     * recognized compile and start scripts. <br />
     * The returned map follows the kernel directory structure.
     * This means the values maybe either maps containing the
     * scripts of a kernel or values are sets containing the
     * subdirectory maps or sets.
     * @return a mapping of kernel directory names to its scripts.
     **/
    public static Map loadKernelList() {
        BIGInterface db = BIGInterface.getInstance();
        Map result = new HashMap();
        Map tempMap = new HashMap();
        // subdirlist is taken from availablekernels.txt
        // this file is generated at every GUI start and deleted after exit
        BIGStrings bs = BIGExecute.getAvailableKernels( new File(db.getBenchItPath() +
                                                  File.separator + "kernel" +
                                                  File.separator )) ;
     /*   String filename = db.getBenchItPath() + File.separator
                          + "gui" + File.separator + "cfg" + File.separator
                          + "availablekernels.txt";

        Object[] linesArray = (readFileLines(filename).toArray());*/
     Object[] linesArray=bs.toArray();
        Arrays.sort(linesArray);
        // filter out empty lines or lines that don't start
        // with the kernel directory
        File[] subdirList = new File[linesArray.length];
        for (int i = 0; i < linesArray.length; i++) {
            String str = db.getBenchItPath() + File.separator
                         + linesArray[i].toString();
            subdirList[i] = new File(str);
        }
        try {
            for (int i = 0; i < subdirList.length; i++) {
                tempMap =
                        checkShellScriptExistance(subdirList[i].getAbsolutePath());
                if (tempMap.size() > 0) {
                    // the current kernel directory has start scripts
                    // take apart the path of that kernel
                    // FIRST: remove "kernel/" from the beginning, since
                    //        all kernels must be in that directory.
                    String kernelPath = linesArray[i].toString();
                    kernelPath = kernelPath.substring(7, kernelPath.length());
                    StringTokenizer st = new StringTokenizer(kernelPath, "/");
                    // SECOND: find occurances of File.separator from left to right
                    ArrayList subDirs = new ArrayList();
                    while (st.hasMoreTokens()) {
                        String tok = st.nextToken();
                        if (st.hasMoreTokens()) {
                            subDirs.add(tok);
                        }
                    }
                    BIGKernel kernel = null;
                        try
                        {
                           kernel = new BIGKernel( subdirList[ i ]
                               /*, r, a, subDirs*/ , tempMap ) ;
                        }
                        catch ( StringIndexOutOfBoundsException ex )
                        {
                           System.err.println(subdirList[ i ] +" is not a valid kernel");
                        }
                    // now add kernel to result map
                    if (kernel != null) {
                        if (subDirs.size() > 0) {
                            // kernel is in one of the kernel subdirectories
                            Map parent = result;
                            Map subMap = null;
                            for (int j = 0; j < subDirs.size(); j++) {
                                String subDirName = (String) subDirs.get(j);
                                // check, if result map contains subDirName as key
                                if (parent.containsKey(subDirName)) {
                                    subMap = (Map) parent.get(subDirName);
                                } else {
                                    subMap = new HashMap();
                                    parent.put(subDirName, subMap);
                                }
                                // for next directory hierarchy level in next loop
                                parent = subMap;
                            }
                            TreeSet kernelSet = null;
                            if (parent.containsKey("<kernels>")) {
                                kernelSet = (TreeSet) parent.get("<kernels>");
                            } else {
                                kernelSet = new TreeSet();
                                parent.put("<kernels>", kernelSet);
                            }
                            // TreesSet elements must implement Comparable
                            kernelSet.add(kernel);
                        } else {
                            // no subdirectories; kernel is in kernel directory
                            TreeSet kernelSet = null;
                            if (result.containsKey("<kernels>")) {
                                kernelSet = (TreeSet) result.get("<kernels>");
                            } else {
                                kernelSet = new TreeSet();
                                result.put("<kernels>", kernelSet);
                            }
                            kernelSet.add(kernel);
                        }
                    }
                }
            }
        } catch (NullPointerException npe) {
            System.err.println(
                    "BIGExecute: loadKernelList: Maybe there are no subdirs listet! "
                    + npe);
        }
        return result;
    }

    /**
     * Returns the names of all available kernels to run.
     *
     * @return          a list of all available kernelnames
     * @deprecated
     **/
    public BIGStrings getKernels() {
        BIGStrings result = new BIGStrings();
        if (kernelList == null) {
            kernelList = loadKernelList();
        }

        Map parent = kernelList;
        Iterator it = parent.keySet().iterator();
        // start adding kernels recursivly
        addKernels(result, parent, it);
        result.sort();
        return result;
    }

    /** Recursive method to add all kernel names of a subdirectory.
     * @deprecated
     * */
    private void addKernels(BIGStrings result, Map parent, Iterator it) {
        while (it.hasNext()) {
            String key = (String) it.next();
            if (key.equals("<kernels>")) {
                Iterator it2 = ((Set) parent.get(key)).iterator();
                while (it2.hasNext()) {
                    result.add(((BIGKernel) it2.next()).getName());
                }
            } else {
                // recurse subdirectories
                Map child = (Map) parent.get(key);
                Iterator it2 = child.keySet().iterator();
                addKernels(result, child, it2);
            }
        }
    }

    /**
     * Retrieves the BIGKernel from the kernelList.
     * @return null, if kernel not found, BIGKernel else.
     * @deprecated
     **/
    public BIGKernel getKernel(String name) {
        Map parent = kernelList;
        Iterator it = parent.keySet().iterator();
        // start adding kernels recursivly
        return findKernel(name, parent, it);
    }

    /**
     * Recursive method to find a specific kernel.
     * @return null, if kernel not found, the BIGKernel else.
     * @deprecated
     **/
    public static BIGKernel findKernel(String kernelName, Map parent,
                                       Iterator it) {
        while (it.hasNext()) {
            String key = (String) it.next();
            if (key.equals("<kernels>")) {
                Iterator it2 = ((Set) parent.get(key)).iterator();
                while (it2.hasNext()) {
                    BIGKernel kernel = (BIGKernel) it2.next();
                    if (kernel.getAbsolutePath().equals(kernelName)) {
                        return kernel;
                    }
                }
            } else {
                // recurse subdirectories
                Map child = (Map) parent.get(key);
                Iterator it2 = child.keySet().iterator();
                BIGKernel k = findKernel(kernelName, child, it2);
                if (k != null) {
                    return k;
                }
            }
        }
        // if this point is ever reached, the kernel was not found
        return null;
    }

    /**
     * Returns the kernel list Map. The returned Map must not be
     * changed.
     **/
    public Map getKernelList() {
        return getKernelList(KERNEL_DIRECTORY);
    }

    /**
     * Returns a kernel list Map for building trees. The returned Map must not be
     * changed. The returned map has a structur according to the sorting
     * parameter. An invalid sorting parameter defaults to KERNEL_DIRECTORY.
     *
     * @param sorting SOURCE_LANGUAGE or KERNEL_DIRECTORY (default).
     * @deprecated
     **/
    public Map getKernelList(int sorting) {
        Map retval = null;
        switch (sorting) {
        case SOURCE_LANGUAGE:
            retval = createSourceSorting();
            break;
        case KERNEL_DIRECTORY:
        default:
            retval = kernelList;
        }
        return retval;
    }

    /**
     * Creates a new Map out of a reloaded kernelList sorted by source language
     * in it's top level and underneath by directory structure. The kernelList
     * is not modified instead a new Map is being build.
     *
     * @return a tree structured map of kernels sorted by source language.
     * @deprecated
     **/
    private Map createSourceSorting() {
        // result is a map with source languages as keys and Maps as values
        Map result = new HashMap();
        // make sure we have a current kernel directory structure
        reloadKernelList();
        Map tempMap = new HashMap();
        // subdirlist is taken from availablekernels.txt
        // this file was generated by reloadKernelList and will be deleted
        // after exit
        String filename = db.getBenchItPath() + File.separator
                          + "gui" + File.separator + "cfg" + File.separator
                          + "availablekernels.txt";
        Object[] linesArray = (readFileLines(filename).toArray());
        Arrays.sort(linesArray);
        // compose absolute path names of the kernel directories
        File[] subdirList = new File[linesArray.length];
        for (int i = 0; i < linesArray.length; i++) {
            String str = db.getBenchItPath() + File.separator
                         + linesArray[i].toString();
            subdirList[i] = new File(str);
        }
        try {
            for (int i = 0; i < subdirList.length; i++) {
                tempMap =
                        checkShellScriptExistance(subdirList[i].getAbsolutePath());
                if (tempMap.size() > 0) {
                    // the current kernel directory has start scripts
                    // take apart the path of that kernel
                    // FIRST: remove "kernel/" from the beginning, since
                    //        all kernels must be in that directory.
                    String kernelPath = linesArray[i].toString();
                    kernelPath = kernelPath.substring(7, kernelPath.length());
                    StringTokenizer st = new StringTokenizer(kernelPath, "/");
                    // SECOND: find occurances of File.separator from left to right
                    ArrayList subDirs = new ArrayList();
                    String token = null;
                    while (st.hasMoreTokens()) {
                        token = st.nextToken();
                        if (st.hasMoreTokens()) {
                            subDirs.add(token);
                        }
                    }
                    // a token can never be null at this point, but you never know
                    if (token == null) {
                        continue;
                    }
                    // the last entry in subDirs is the name of the kernel directory
                    // where the kernel files are in -> extract the source language
                    String language = extractSourceLanguage(token);
                    // in the case the language couldn't be extracted, place those
                    // kernels under the "unknown" node
                    if (language == null) {
                        language = "unknown";
                    }
                    // make all languages to lower case
                    language = language.toLowerCase();
                    // set of values for this key
                    Map sourceMap = null;
                    if (result.containsKey(language)) {
                        sourceMap = (Map) result.get(language);
                    } else {
                        sourceMap = new HashMap();
                        result.put(language, sourceMap);
                    }
                    // get the kernel object from the kernelList
                    BIGKernel kernel = getKernel(subdirList[i].getName());
                    // now add kernel to result map
                    if (kernel != null) {
                        if (subDirs.size() > 0) {
                            // kernel is in one of the kernel subdirectories
                            Map parent = sourceMap;
                            Map subMap = null;
                            for (int j = 0; j < subDirs.size(); j++) {
                                String subDirName = (String) subDirs.get(j);
                                // check, if result map contains subDirName as key
                                if (parent.containsKey(subDirName)) {
                                    subMap = (Map) parent.get(subDirName);
                                } else {
                                    subMap = new HashMap();
                                    parent.put(subDirName, subMap);
                                }
                                // for next directory hierarchy level in next loop
                                parent = subMap;
                            }
                            TreeSet kernelSet = null;
                            if (parent.containsKey("<kernels>")) {
                                kernelSet = (TreeSet) parent.get("<kernels>");
                            } else {
                                kernelSet = new TreeSet();
                                parent.put("<kernels>", kernelSet);
                            }
                            // TreesSet elements must implement Comparable
                            kernelSet.add(kernel);
                        } else {
                            // no subdirectories; kernel is in kernel directory
                            TreeSet kernelSet = null;
                            if (result.containsKey("<kernels>")) {
                                kernelSet = (TreeSet) sourceMap.get("<kernels>");
                            } else {
                                kernelSet = new TreeSet();
                                sourceMap.put("<kernels>", kernelSet);
                            }
                            kernelSet.add(kernel);
                        }
                    }
                }
            }
        } catch (NullPointerException npe) {
            System.err.println(
                    "BIGExecute: loadKernelList: Maybe there are no subdirs listet! "
                    + npe);
        }
        return result;
    }

    /**
     * Extracts the the source language out of the last path component.
     * Assumes, that the last path component is the directory containing
     * all the kernel's files and that it follows BenchIT kernel directory
     * naming conventions: <name>_<source-language>_<other descriptions>
     *
     * @return the source language of last kernel directory's component, or null on error.
     * @deprecated
     **/
    private String extractSourceLanguage(String path) {
        String retval = null;
        int s = path.indexOf("_") + 1;
        int e = path.indexOf("_", s);
        // a kernel directory name may end right after the source language
        if ((s >= 0) && (e < 0)) {
            // so set the end index to the end of the string
            e = path.length();
        }
        if ((s >= 0) && (e >= 0) && (e > s)) {
            retval = path.substring(s, e);
            // now check, if retval represents a valid source language
            // this info is stored in BGUI.cfg and loaded into a HashSet
            // during BIGExecute instanciation
            // if we found something we don't recognize as a source language,
            // we don't accept it as one
            if (!languages.contains(retval)) {
                retval = null;
            }
        }
        return retval;
    }

    /**
     * Checks the existance of MAINDIREXEC.
     *
     * @return          Is true when MAINDIREXEC.SH exists
     **/
    public boolean existsMainDirExec() {
        if (kernelList == null) {
            kernelList = loadKernelList();
        }
        return kernelList.containsKey(ALL_INDICATOR);
    }

    /**
     * Returns the all indicator to replace it, if needed.
     **/
    public String getAllIndicator() {
        return ALL_INDICATOR;
    }

    public BIGStrings getShellScriptList() {
        return shellScriptList;
    }

    /**
     * Checks the existance of a shell script in a specific kernel.
     *
     * @param   kernelName     name of the kernel where to search.
     * @param   scriptName     name of the script to search for.
     * @return                 Is true when the script exists in the kernel.
     * @deprecated
     **/
    public boolean existsScripsForKernel(
            String kernelName, String scriptName) {
        boolean result = false;

        Map parent = kernelList;
        Iterator it = parent.keySet().iterator();
        // recursivly search for kernel
        BIGKernel k = findKernel(kernelName, parent, it);
        if (k == null) {
            result = false;
        } else {
            Map scripts = k.getScripts();
            if (scripts == null) {
                result = false;
            } else {
                String cmd = (String) scripts.get(scriptName);
                if (cmd == null) {
                    result = false;
                } else {
                    result = true;
                }
            }
        }
        return result;
    }

    /**
     * Starts all kernels.
     * It uses <code>Maindirexec.sh</code>.
     *
     **/
//   public void startKernel() {
    // there is no more MAINDIREXEC.SH
//      startKernel(ALL_INDICATOR, "MAINDIREXEC.SH");
//   }

    /**
     * Starts a specific kernel. Default shell script will be run.
     *
     * @param   kernelName  name of the kernel to start
     * @deprecated
     **/
    public void startKernel(String kernelName) {
        startKernel(kernelName, DEFAULT_SCRIPT);
    }

    /**
     * Starts a specific kernel.
     *
     * @param   kernelName  name of the kernel to start
     * @param   scriptName  name of the shell script to start
     * @deprecated
     **/
    public void startKernel(String kernelName, String scriptName) {
        if (running == true) {
            System.err.println(
                    "BIGExecute: there already a process is running.");
        } else {
            // find the map corresponding to the kernelName
            Map parent = kernelList;
            Iterator it = parent.keySet().iterator();
            // recursivly search for kernel
            BIGKernel k = findKernel(kernelName, parent, it);
            if (debug)System.err.println("------startKernel(..)-------");
            if (k == null) {
                System.err.println(
                        "BIGExecute: This kernel was not found: " + kernelName);
                return;
            }
            Map scripts = k.getScripts();
            if (debug)System.err.println("Scripts:"+scripts);
            if (scripts == null) {
                System.err.println(
                        "BIGExecute: This is not a valid kernel: " + kernelName);
                return;
            } else {
               String cmd = (String) scripts.get(scriptName);
            if (debug)System.err.println("Cmd:"+cmd);
                if (cmd == null) {
                    System.err.println(
                            "BIGExecute: This is not a valid script: "
                            + scriptName);
                } else {
                    execute(cmd);
                }
            }
        }
    }

    /**
     * generate the file postproc.sh
     * this is executed via "source postproc.sh"
     * in BenchIT (shellscript)
     * postproc contains commands for starting the kernels
     * this is for benchmarking without java in the background.<br>
     * Both parameter BIGStrings should have the same length, for every
     * kernel one script to execute, kernels and scripts can exist twice
     * in the BIGStrings.
     *
     * @param kernels      the kernels to be executed
     * @param scripts      the scripts to be executed on the kernel
 //    * @deprecated
     **/
/*    public boolean generatePostProcScript(
            BIGStrings kernels, BIGStrings scripts) {

               if (debug)System.err.println("kernels:\n"+kernels.toString());
                if (debug)System.err.println("scripts:\n"+kernels.toString());
        BIGStrings script = new BIGStrings();
        if (kernels.size() != scripts.size()) {
            System.err.println(
                    "BIGExecute: Number of kernels unequal to number of scripts.");
            while (kernels.size() > scripts.size()) {
                scripts.add(DEFAULT_SCRIPT);
            }
        }
        // header of file
        script.add("#!/bin/sh");
        script.add("echo \"##### starting postproceeding / selected kernels\"");

        // kernel starts
        for (int pos = 0; pos < kernels.size(); pos++) {
            String kernelName = kernels.get(pos);
            String scriptName = scripts.get(pos);
            Map parent = kernelList;
            Iterator it = parent.keySet().iterator();
            // recursivly search for kernel
            BIGKernel k = findKernel(kernelName, parent, it);
            if (k == null) {
                System.err.println("BIGExecute: couldn't find kernel "
                                   + kernelName);
                continue;
            }
            Map scrpts = k.getScripts();
            if (scrpts == null) {
                System.err.println("BIGExecute: This is not a valid kernel: "
                                   + kernelName);
                continue ;
             }
             else
             {
                 if (debug)System.err.println( "ScriptName:" + scriptName ) ;
                String cmd = ( String ) scrpts.get( scriptName ) ;
                if ( ( cmd == null ) &&
                     ( ! ( ( scriptName.equals( this.BOTH ) ) ||
                           ( scriptName.equals( this.COMPILE ) ) ||
                           ( scriptName.equals( this.RUN ) ) ) ) )
                {
                   System.err.println(
                       "BIGExecute: This is not a valid script "
                       + scriptName + " for " + kernelName + "." + cmd ) ;
                    continue;
                } else {
                    script.add("echo \"##### starting \\\"" + kernelName +
                               "\\\"\"");
                    if (scriptName.equals(this.BOTH)) {


                       String standardComment=null;
                       String target=null;
                       String noParam="";
                       try
                       {
                          if ( BIGInterface.getInstance().
                               getBIGConfigFileParser().boolCheckOut(
                                   "standardCommentActive" )  )
                          {
                             standardComment =
                                 BIGInterface.getInstance().
                                 getBIGConfigFileParser().
                                 stringCheckOut(
                                     "standardComment" ) ;
                          }
                          if ( BIGInterface.getInstance().
                               getBIGConfigFileParser().boolCheckOut(
                                   "settedTargetActiveCompile" )  )
                          {
                             target = " --target=" +
                                 BIGInterface.getInstance().
                                 getBIGConfigFileParser().
                                 stringCheckOut(
                                     "settedTargetCompile" ) ;
                          }
                          if ( BIGInterface.getInstance().
                               getBIGConfigFileParser().boolCheckOut(
                                   "settedTargetActiveCompile" )  )
                          {
                             noParam = "--no-parameter-file ";
                          }


                       }
                       catch ( Exception e )
                       {
                          System.err.println(
                              "did not find \"standardComment(Active) in BGUI.cfg\"" ) ;

                       }
                       String compile=BIGInterface.getInstance().getBenchItPath() +
                           File.separator + "COMPILE.SH";
                       if (standardComment!=null)
                          compile=compile+" --comment="+standardComment;
                       if (target!=null)
                          compile=compile+target;
                       compile=compile+" "+k.getNameAfterSorting(0);
                       script.add( compile ) ;
                       String run = BIGInterface.getInstance().getBenchItPath() +
                           File.separator + "RUN.SH";
                       if ( target != null )
                          run = run + target ;
                       run=run+noParam;
                       run=run+
                       " "+
                           k.getNameAfterSorting( 0 ) + "." ;
                       if ( standardComment == null )
                          run = run + "0" ;
                       else
                          run = run + standardComment ;

                       script.add( run) ;
                    } else {
                       if (scriptName.equals(this.COMPILE))
                       {
                          String add="";
                          try
                          {
                             if ( BIGInterface.getInstance().
                                  getBIGConfigFileParser().intCheckOut(
                                      "standardCommentActive" ) > 0 )
                             {
                                add = " --comment=" +
                                    BIGInterface.getInstance().
                                    getBIGConfigFileParser().
                                    stringCheckOut(
                                        "standardComment" ) ;
                             }
                             if ( BIGInterface.getInstance().
                                  getBIGConfigFileParser().intCheckOut(
                                      "settedTargetActiveCompile" ) > 0 )
                             {
                                add = add+" --target=" +
                                    BIGInterface.getInstance().
                                    getBIGConfigFileParser().
                                    stringCheckOut(
                                        "settedTargetCompile" ) ;
                             }
                             String compile = BIGInterface.getInstance().
                                 getBenchItPath() +
                                 File.separator + "COMPILE.SH" + add + " " +
                                 k.getNameAfterSorting( 0 ) ;
                             script.add( compile ) ;

                          } catch (Exception e)
                          {
                             System.err.println(
                                 "did not find \"standardComment(Active) in BGUI.cfg\"" ) ;
                             script.add( cmd ) ;
                          }

                       } else
                       {
                          if (k.getNumberOfCompilations()>1)
                          {
                              if (debug)System.err.println("RUN and more then one kernel");
                             String[] kernelExec=k.askWhichKernel( null ) ;
                             if (kernelExec==null)
                                return false;
                             for (int i=0;i<kernelExec.length;i++)
                             {
                                String add = "" ;
                                try
                                {
                                   if ( BIGInterface.getInstance().
                                        getBIGConfigFileParser().intCheckOut(
                                            "settedTargetActiveRun" ) > 0 )
                                   {
                                      add = " --target=" +
                                          BIGInterface.getInstance().
                                          getBIGConfigFileParser().
                                          stringCheckOut(
                                              "settedTargetRun" ) ;
                                   }
                                   if ( BIGInterface.getInstance().
                                        getBIGConfigFileParser().boolCheckOut(
                                            "settedTargetActiveCompile" ) )
                                   {
                                      add=add+" --no-parameter-file" ;
                                   }

                                }
                                catch ( Exception ex )
                                {
                                   System.err.println(
                                       "Did not find settedTarget(Active)Run in BGUI.cfg" ) ;
                                }

                                script.add( BIGInterface.getInstance().
                                            getBenchItPath() + File.separator+"RUN.SH"+add+" "+
                                            kernelExec[ i ]) ;
                                script.add(
                                    "echo \"##### finished \\\"" + kernelName +
                                    "(" + kernelExec[ i ] + ")" +
                                    "\\\"\"" ) ;

                             }
                          }
                           else
                              if (k.getNumberOfCompilations()==0)
                              {
                                 System.out.println(
                                     "Please compile the kernel " + k.getName() +
                                     "\nbefore trying to run it." ) ;
                                    return false;
                              }
                            else
                            {
                                String add = "" ;
                                try
                                {
                                   if ( BIGInterface.getInstance().
                                        getBIGConfigFileParser().intCheckOut(
                                            "settedTargetActiveRun" ) > 0 )
                                   {
                                      add = " --target=" +
                                          BIGInterface.getInstance().
                                          getBIGConfigFileParser().
                                          stringCheckOut(
                                              "settedTargetRun" ) ;
                                   }
                                   if ( BIGInterface.getInstance().
                                        getBIGConfigFileParser().boolCheckOut(
                                            "settedTargetActiveCompile" ) )
                                   {
                                      add=add+" --no-parameter-file" ;
                                   }

                                }
                                catch ( Exception ex )
                                {
                                   System.err.println(
                                       "Did not find settedTarget(Active)Run in BGUI.cfg" ) ;
                                }

                               script.add( BIGInterface.getInstance().
                                           getBenchItPath() + File.separator +"RUN.SH"+add+" "+
                                           k.getCompiledNames()[ 0 ] ) ;
                            }
                       }

                    }
                    script.add(
                        "echo \"##### finished \\\"" + kernelName +
                        "(all Selected)" +
                        "\\\"\"" ) ;
                }
            }
        }

        //"restart-GUI"-command
        script.add("echo \"##### finished postproceeding - restarting GUI\"");
        script.add("sh GUI.sh -restart");

        // write file
        try {
            script.saveToFile("postproc.sh");
        } catch (Exception e) {
            System.err.println(
                    "BIGExecute: Error during writing of postproc.sh\n"
                    + e);
              }
              return true;
    }*/


    /**
     * generate the file postproc.sh
     * this is executed via "source postproc.sh"
     * in BenchIT (shellscript)
     * postproc contains commands for starting the kernels
     * this is for benchmarking without java in the background.<br>
     * Both parameter BIGStrings should have the same length, for every
     * kernel one script to execute, kernels and scripts can exist twice
     * in the BIGStrings.
     *
     * @param kernels      the kernels to be executed
     * @param scripts      the scripts to be executed on the kernel
     * @param fileName     use this as fileName "postProc.sh" when shutting down GUI
     **/
    public String generatePostProcScript(
            BIGStrings kernels, BIGStrings scripts, String fileName, gui.BIGKernelTree kernelTree, boolean restartGui)
         {
            String comment = BIGInterface.getInstance().getEntry( "<hostname>" ,
                "BENCHIT_FILENAME_COMMENT" ).getValue().toString() ;
            if ( !BIGInterface.getInstance().getOriginalHost().equals( BIGInterface.
                getInstance().getHost() ) )
            {
               comment = BIGFileHelper.getFileContent( new File( BIGInterface.
                   getInstance().getBenchItPath() + File.separator + "LOCALDEFS" +
                   File.separator + BIGInterface.getInstance().getOriginalHost() ) ) ;
               if ( comment != null )
                  comment = plot.BIGOutputParser.getValue( "BENCHIT_FILENAME_COMMENT" ,
                      comment ) ;
            }
            String standardComment = null ;
            if ((comment!=null)||(!comment.equals("")))
               standardComment=comment;
            String target = "" ;
            String noParam = "" ;


             if (debug)System.err.println("---------genBckPostProc(..)------------");
        if (fileName==null) fileName = "postproc"+System.currentTimeMillis()+".sh";

        BIGStrings script = new BIGStrings();
        if (kernels.size() != scripts.size()) {
            System.err.println(
                    "BIGExecute: Number of kernels unequal to number of scripts.");
            while (kernels.size() > scripts.size()) {
                scripts.add(DEFAULT_SCRIPT);
            }
        }

        // header of file
        script.add("#!/bin/sh");
        script.add("echo \"##### starting postproceeding / selected kernels\"");

        // kernel starts
        for (int pos = 0; pos < kernels.size(); pos++) {
            String kernelName = kernels.get(pos);
             if (debug)System.err.println("kernelName:"+kernelName);
            String scriptName = scripts.get(pos);
             if (debug)System.err.println("script:"+scriptName);
            //Map parent = kernelList;
            //Iterator it = parent.keySet().iterator();
            // recursivly search for kernel
            BIGKernel k = kernelTree.findKernel(kernelName);
            if (k == null) {
                System.err.println("BIGExecute: couldn't find kernel "
                                   + kernelName);
                continue;
            }
            // maybe the comment is set in PARAMETERS
            File files[]=k.getFiles();
            for (int l=0;l<files.length;l++)
            {
               if (files[l].getName().equals("PARAMETERS"))
               {
                  String paramComment = plot.BIGOutputParser.getValue(
                      "BENCHIT_FILENAME_COMMENT" ,
                      BIGFileHelper.getFileContent(files[ l ] ) ) ;
                  if ((paramComment!=null)&&(!paramComment.equals("")))
                  {
                     standardComment=paramComment;
                  }
               }
            }
/*            Map scrpts = k.getScripts();
            if (scrpts == null) {
                System.err.println("BIGExecute: This is not a valid kernel: "
                                   + kernelName);
                continue;
             } else */{
                String cmd = scriptName  ;
                if ( ( cmd == null ) &&
                     ( ! ( ( scriptName.equals( this.BOTH ) ) ||
                       ( scriptName.equals( this.COMPILE ) )  ||
                       ( scriptName.equals( this.RUN ) ) ) ) )
                {
                    System.err.println(
                            "BIGExecute: This is not a valid script "
                            + scriptName + " for " + kernelName + "." + cmd);
                    continue;
                } else {
                    script.add("echo \"##### starting \\\"" + kernelName +
                               "\\\"\"");
                    if (scriptName.equals(this.BOTH))
                    {

                       try
                       {
                          if ( BIGInterface.getInstance().
                               getBIGConfigFileParser().intCheckOut(
                                   "settedTargetActiveCompile" ) > 0 )
                          {
                             target = " --target=" +
                                 BIGInterface.getInstance().
                                 getBIGConfigFileParser().
                                 stringCheckOut(
                                     "settedTargetCompile" ) ;
                          }
                          if ( BIGInterface.getInstance().
                               getBIGConfigFileParser().boolCheckOut(
                                   "runWithoutParameter" ) )
                          {
                             noParam = " --no-parameter-file" ;
                          }

                       }
                       catch ( Exception e )
                       {
                          System.err.println(
                              "did not find \"standardComment(Active) in BGUI.cfg\"" ) ;

                       }
                       String compile=BIGInterface.getInstance().getBenchItPath() +
                           File.separator + "COMPILE.SH";
                       if (target!=null)
                          compile=compile+target;
                       compile=compile+" "+k.getNameAfterSorting(0);
                       script.add( compile ) ;

                       String run = BIGInterface.getInstance().getBenchItPath() +
                           File.separator +"RUN.SH";
                       run = run + target ;
                       run=run+noParam;
                       run=run+" "+
                           k.getNameAfterSorting( 0 ) + "." ;
                       if (( standardComment == null )||(standardComment.equals("")))
                          run = run + "0" ;
                       else
                          run = run + standardComment ;

                       script.add( run) ;
                     }  else
                        if ( scriptName.equals( this.COMPILE ) )
                        {
                            if (debug)System.err.println("Compile");
                           String add = "" ;
                           try
                           {
                              if ( BIGInterface.getInstance().
                                   getBIGConfigFileParser().intCheckOut(
                                       "settedTargetActiveCompile" ) > 0 )
                              {
                                 add = add+" --target=" +
                                     BIGInterface.getInstance().
                                     getBIGConfigFileParser().
                                     stringCheckOut(
                                         "settedTargetCompile" ) ;
                            if (debug)System.err.println("Add2:"+add);
                         }
                        String compile = BIGInterface.getInstance().
                            getBenchItPath() +
                            File.separator + "COMPILE.SH" + add + " " +
                            k.getNameAfterSorting( 0 ) ;
                        script.add( compile ) ;

                           }
                           catch ( Exception e )
                           {
                              System.err.println(
                                  "did not find \"settedTarget(Active) in BGUI.cfg\"" ) ;
                              script.add( cmd ) ;
                           }

                        } else
                        {
                           if (k.getNumberOfCompilations()>1)
                          {
                             String[] kernelExec=k.askWhichKernel( null ) ;
                             if (kernelExec==null)
                                return null;
                              try
                              {
                                 if ( BIGInterface.getInstance().
                                      getBIGConfigFileParser().boolCheckOut(
                                          "runWithoutParameter" ) )
                                 {
                                    noParam = " --no-parameter-file" ;
                                 }
                              }
                              catch ( Exception ex1 )
                              {
                              }

                             for (int i=0;i<kernelExec.length;i++)
                             {

                                script.add( BIGInterface.getInstance().
                                            getBenchItPath() + File.separator+"RUN.SH"+noParam +" "+kernelExec[i] ) ;

                                script.add(
                                    "echo \"##### finished \\\"" + kernelName +
                                    "(" + kernelExec[ i ] + ")" +
                                    "\\\"\"" ) ;

                             }
                          }

                          else
                          if ( k.getNumberOfCompilations() == 0 )
                          {
                             System.out.println(
                                 "Please compile the kernel " + k.getName() +
                                 "\nbefore trying to run it." ) ;
                                    return null;
                          }
                           else
                           {
                              String add = "" ;
                              try
                              {
                                 if ( BIGInterface.getInstance().
                                      getBIGConfigFileParser().intCheckOut(
                                          "settedTargetActiveCompile" ) > 0 )
                                 {
                                    add = " --target=" +
                                        BIGInterface.getInstance().
                                        getBIGConfigFileParser().
                                        stringCheckOut(
                                            "settedTargetCompile" ) ;
                                 }
                                 if ( BIGInterface.getInstance().
                                      getBIGConfigFileParser().boolCheckOut(
                                          "runWithoutParameter" ) )
                                 {
                                    add = add+" --no-parameter-file" ;
                                 }

                              }
                              catch ( Exception ex )
                              {
                                 System.err.println(
                                     "Did not find settedTarget(Active)Compile in BGUI.cfg" ) ;
                              }

                              script.add( BIGInterface.getInstance().
                                          getBenchItPath() + File.separator +"RUN.SH"+add+" "+
                                          k.getCompiledNames()[ 0 ] ) ;
                           }

                        }
                        script.add(
                            "echo \"##### finished \\\"" + kernelName +
                            "(all Selected)" +
                            "\\\"\"" ) ;
                        script.add(
                            "echo \"##### removing shellscript \"\n" ) ;
                        script.add(
                           "rm -f "+fileName ) ;
                }
            }
        }
        if (restartGui)
           script.add("./GUI.sh");
        String binPath = db.getBenchItPath() + File.separator
            + "gui" + File.separator + "bin" + File.separator ;
        // write file
        try
        {
           script.saveToFile( binPath + fileName ) ;
        }
        catch ( Exception e )
        {
           System.err.println(
               "BIGExecute: Error during writing of postproc.sh\n"
               + e ) ;
        }

        return fileName;
    }

    /**
     * get a BIGStrings of all available outputfiles
     *
     * @return BIGStrings - the name of all outputfiles (*.bit)
     * @see BIGStrings
     **/
    public BIGStrings getResultList() {
        BIGStrings list = new BIGStrings();
        String name;
        File outputDir = new File(db.getInstance().getBenchItPath()
                                  + File.separator + "output");
        File[] preList = outputDir.listFiles();
        if (preList == null) {
            return new BIGStrings();
        }
        if (debug) {
            System.out.println("BIGExecute: results found:");
        }
        for (int i = 0; i < preList.length; i++) {
            name = preList[i].getName();
            if (name.endsWith(".bit")) {
                list.add(name);
                if (debug) {
                    System.out.println("BIGExecute: \t" + name);
                }
            }
        }
        if (debug) {
            System.out.println("BIGExecute: results found:");
        }
        list.sort();
        return list;
    }

    /**
     * execute QUICKVIEW.SH with all outputfiles in list
     *
     * @param list a list of valid outputfiles located in /output
     * @deprecated
     **/
    public void showResult(BIGStrings list) {
        String s = new String();
        Iterator it = list.iterator();
        while (it.hasNext()) {
            s += " ";
            s += it.next();
        }
        String cmd = db.getBenchItPath() + File.separator
                     + "tools" + File.separator + "QUICKVIEW.SH " + s;
        if (debug) {
            System.out.println("BIGExecute: executing:" + cmd);
        }
        this.execute(cmd, true);
    }

    /** Reads a file line by line and stores the lines as entries in
     * a HashSet.
     * @return returns a HashSet of lines of a file.
     * @deprecated
     **/
    private static HashSet readFileLines(String filename) {
        HashSet retval = new HashSet();
        File textFile = null;
        try {
            textFile = new File(filename);
        } catch (NullPointerException npe) {}
        if (textFile != null) {
            if (textFile.exists()) {
                char ch = '\0';
                int size = (int) (textFile.length());
                int read = 0;
                char inData[] = new char[size + 1];
                // read file content into the inData buffer
                try {
                    BufferedReader in = new BufferedReader(
                            new FileReader(textFile));
                    in.read(inData, 0, size);
                    in.close();
                } catch (IOException ex) {}
                // scan inData for lines
                int scanned = 0;
                int it = 0;
                ch = inData[scanned];
                StringBuffer buffer = new StringBuffer();
                while (scanned < size) {
                    // read a line
                    while (ch != '\n') {
                        if (ch != '\r') {
                            buffer.append(ch);
                        }
                        scanned += 1;
                        ch = inData[scanned];
                    }
                    String lineValue = buffer.toString();
                    retval.add(lineValue);
                    scanned += 1;
                    ch = inData[scanned];
                    buffer = new StringBuffer();
                }
            }
        }
        return retval;
    }


/*    public static void createAvailableKernelsFile(File kernelDirectory)

    {

       BIGInterface bigInterface = BIGInterface.getInstance();
       File avKernels = new File(
               bigInterface.getBenchItPath()
               + File.separator + "gui" + File.separator + "cfg"
               + File.separator + "availablekernels.txt");
       avKernels.deleteOnExit();

       //System.err.println("KernelDir:"+kernelDirectory);
       if (kernelDirectory==null)
       {
          System.err.println("No kernelDirectory given, using default");
          kernelDirectory=new File(bigInterface.getBenchItPath()+File.separator+"kernel"+File.separator);
       }
       BIGStrings bs=getAvailableKernels(kernelDirectory);
       /*for (int i=0;i<subFiles.length;i++)
       {
          if (subFiles[i].isDirectory())

               }
       System.err.println("bs:"+bs.toString());
       try {
       DataOutputStream bos = new DataOutputStream(
           new FileOutputStream( avKernels ) ) ;
           bos.write(bs.toString().getBytes());
           bos.close();
            } catch (IOException ex) {
                System.err.println(
                        "Couldn't write kernels to availablekernels.txt");
            }


    }*/

/**
 *
 * @param path File
 * @return BIGStrings
 * @deprecated
 */
private static BIGStrings getAvailableKernels(File path)
    {
       int length =BIGInterface.getInstance().getBenchItPath().length();
       if (BIGInterface.getInstance().getBenchItPath().endsWith(File.separator))
          length++;
       //System.err.println("checking dir:"+path);
       BIGStrings bs=new BIGStrings();
       File[] subFiles= path.listFiles(new FileFilter()
       {
          public boolean accept(File f)
          {
             if (f.isDirectory())
                if (!f.getName().equals("CVS"))
                   return true;
          //   if (f.getName().equals("COMPILE.SH"))
          //      return true;
             return false;
          }
       });
       if (subFiles==null)
          return new BIGStrings();
       for (int i=0;i<subFiles.length;i++)
       {
          if (subFiles[i].isDirectory())
          {
             bs.add(subFiles[i].getAbsolutePath().substring(length));
             bs=bs.concat(BIGExecute.getAvailableKernels(subFiles[i]));

          }
          /*else
          {
             System.err.println( path.getAbsolutePath().substring(length) ) ;
             bs.add( path.getAbsolutePath().substring(length) ) ;
          }*/
       }
       return bs;



    }

       /*
        if (!avKernels.exists()) {
            try {
                DataOutputStream bos = new DataOutputStream(
                        new FileOutputStream(avKernels));
                File[] kernelSubDirs = (new File(bigInterface.getBenchItPath()
                                                 + File.separator + "kernel")).
                                       listFiles(new FileFilter() {
                    public boolean accept(File f) {
                        if (f.isDirectory()) {
                            if (f.getName().equals("CVS")) {
                                return false;
                            } else {
                                return true;
                            }
                        } else {
                            return true;
                        }
                    }
                });
                java.util.Vector avKs = new java.util.Vector();
                for (int i = 0; i < kernelSubDirs.length; i++) {
                    avKs.add(kernelSubDirs[i]);
                } while (avKs.size() > 0) {
                    File[] subsubDirs = ((File) avKs.get(0)).listFiles(new
                            FileFilter() {
                        public boolean accept(java.io.File f) {
                            if (f.isDirectory()) {
                                if (f.getName().equals("CVS")) {
                                    return false;
                                } else {
                                    return true;
                                }
                            }
                            return false;
                        }
                    });
                    for (int j = 0; j < subsubDirs.length; j++) {
                        avKs.add(j + 1, subsubDirs[j]);
                    }
                    File[] existPar = ((File) avKs.get(0)).listFiles(new
                            FileFilter() {
                        public boolean accept(java.io.File f) {
                            if (f.isDirectory()) {
                                return false;
                            }
                            if (f.getName().equals("RUN.SH")) {
                                return true;
                            }
                            return false;
                        }
                    });
                    try {
                        String toWrite = ((File) avKs.get(0)).getCanonicalPath().
                                         substring(((File) avKs.get(0)).
                                getCanonicalPath().indexOf("kernel"));
                        if (bigInterface.getSystem() ==
                            bigInterface.WINDOWS_SYSTEM) {
                            toWrite = toWrite.replace('\\', '/');
                        }
                        bos.write(toWrite.getBytes());
                        if (bigInterface.getSystem() ==
                            bigInterface.WINDOWS_SYSTEM) {
                            bos.write("\r".getBytes());
                        }
                        bos.write("\n".getBytes());
                    } catch (IOException ioe) {
                        System.err.println(
                                "Could not write Canonical Path for " +
                                avKs.get(0) + " to availableKernels.txt");
                    }
                    avKs.remove(0);
                }
                bos.close();
            } catch (IOException ex) {
                System.err.println(
                        "Couldn't write kernels to availablekernels.txt");
            }
        }
    }*/

}

/*****************************************************************************
 Log-History

 *****************************************************************************/

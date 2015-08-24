/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  Main program of the BenchIT-project (java version)                         *
 *
 *  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
 *  Last change by: $Author: molka $
 *  $Revision: 1.22 $
 *  $Date: 2007/02/01 15:38:01 $
 *
 ******************************************************************************/

import java.util.*;
import java.io.*;
import java.lang.*;
import java.text.*;

public class JBI {
    public class AxisData {
        public double min, max;
        public double plotmin, plotmax;
        public int base;
        public int ticks;
        public double incr;
        public String name;

        public AxisData() {
            reset();
        }

        public AxisData( String name ) {
            reset();
            this.name = name;
        }

        public void reset() {
            min = 0.0;
            max = 0.0;
            plotmin = 0.0;
            plotmax = 0.0;
            base = 0;
            ticks = 0;
            incr = 0.0;
            name = "";
        }

        public void computeAxisProperties() {
            if( base == 0 ) { // linear scale
                double diff, tickspacing;

                diff = max - min;
                if( diff <= 0.0 ) {
                    return;
                }

                plotmin = 0.0;
                tickspacing = 1.0;
                if( diff > 10.0 ) {
                    while( diff > 10.0 ) {
                        diff /= 10.0;
                        tickspacing *= 10.0;
                    }
                } else {
                    while( diff < 1.0 ) {
                        diff *= 10.0;
                        tickspacing /= 10.0;
                    }
                }
                if( diff <= 2.5 ) {
                    tickspacing /= 4.0;
                } else if( diff <= 5.0 ) {
                    tickspacing /= 2.0;
                }
                incr = tickspacing;
                while( plotmin <= ( min - tickspacing ) ) {
                    plotmin += tickspacing;
                }

                plotmax = plotmin;
                ticks = 0;
                while( plotmax < max ) {
                    plotmax += tickspacing;
                    ticks += 1;
                }
            } else { // logarithmic scale
                plotmin = base;
                if( plotmin > min ) {
                    while( plotmin > min ) {
                        plotmin /= base;
                    }
                } else {
                    while( plotmin <= ( min / base ) ) {
                        plotmin *= base;
                    }
                }
                ticks = 0;
                incr = base;
                plotmax = plotmin;
                while( plotmax < max ) {
                    plotmax *= base;
                    ticks += 1;
                } while( ticks > 10 ) {
                    if( ( ticks & 0x1 ) == 0x1 ) {
                        ticks++;
                    }
                    ticks /= 2;
                    incr *= incr;
                }
            }
        }
    }


    public class ScalingVars {
        public int scalingLevel;
        public double scalingValue;

        public ScalingVars() {
            scalingLevel = 0;
            scalingValue = 1.0;
        }
    }


    static final String BIN_pref[] = {"", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei"};
    static final int BIN_PREF_MAX = 6;
    static final String SI_prefixes[] = {"y", "z", "a", "f", "p", "n", "u", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"};
    static final int SI_PREF_MIN = -8;
    static final int SI_PREF_MAX = 8;
    static final int SI_PREF_ZERO = 8; 

    private BIJavaKernel kernel = null;
    private String hname = null;
    private String nname = null;
    private String kernelstring = null;
    private String outputDir = null;
    private String kernelName = null;
    private String language = null;
    private String[] libraries = null;
    private RWDate rwd = null;
    private BIEnvHash rwe = null;
    private int ERROR_CORRECTION;
    // this value substitutes Infinity and NaN entries in result vectors
    private static final double INVALID_MEASUREMENT = -7.77E7;
    /*
       private double xmin;
       private double xmax;
       private double xoutmin;
       private double xoutmax;
       private int xticks;
       private double[] ymin;
       private double[] ymax;
       private double youtmin[];
       private double youtmax[];
       private int yticks[];
     */
    private AxisData xdata = new AxisData( "x" );
    private AxisData ydata_global = new AxisData( "y" );
    private AxisData[] ydata = null;
    private int numabscissae;
    private double[] allresults = null;
    private boolean timelimit = true;
    private boolean vampireTrace = false;
    private boolean linearMeasurement = false;
    private boolean verbose = false;
    private String subscriber = null;
    private int debugLevel = 0;
    private int[] m_nextProblemTODO;
    private boolean[] m_nextProblemDone;
    private int m_nextProblemMax;
    private static boolean jni=true;

    public JBI() {
        rwe = BIEnvHash.getInstance();
        hname = rwe.bi_getEnv( "BENCHIT_HOSTNAME" );
        nname = rwe.bi_getEnv( "BENCHIT_NODENAME" );
        kernelstring = rwe.bi_getEnv( "BENCHIT_KERNELNAME" );
        if( kernelstring == null ) {
            System.err.println( "BenchIT: Error: BENCHIT_KERNELNAME not set! " );
            System.exit( 1 );
        }
        outputDir = rwe.bi_getEnv( "BENCHIT_RUN_OUTPUT_DIR" );
        timelimit = true;
        ERROR_CORRECTION = 1;
        subscriber = new String();
        debugLevel = 0;
        vampireTrace = false;
        linearMeasurement = false;
	try {
		System.loadLibrary("jbi");
	}
	catch (Throwable t)
	{
		jni=false;
	}
	if (jni==true)
	{
		System.out.println("found C-Timerlibrary");
	}
	else
	{
		System.out.println("C-Timerlibrary not found -> fallback to Java Timerimplementation");
	}
    }

    public static void main( String[] args ) {
        JBI jbi = new JBI();
        jbi.checkCommandLine( args );
        jbi.go();
    }

    public void initialize( BIJavaKernel kernel ) {
        this.kernel = kernel;
        /*      this.xmin = 0.0;
              this.xmax = 0.0;
              this.xoutmin = 0.0;
              this.xoutmax = 0.0;
              this.xticks = 0;
         */
        this.numabscissae = 0;
    }

    public void go() {
        int i = 0;
        kernelName = rwe.bi_getEnv( "BENCHIT_JAVAKERNELCLASS" );
        BIJavaKernel kernel = null;
        Class c = null;
        if( ( hname == null ) || ( nname == null ) || ( kernelstring == null ) || ( outputDir == null ) ) {
            System.out.println( "Set the required environment variables:" );
            System.out.println( "\tBENCHIT_HOSTNAME, BENCHIT_NODENAME, BENCHIT_KERNELNAME, and BENCHIT_JAVAKERNELCLASS" );
            System.out.println( "\tMaybe even the default BENCHIT_RUN_OUTPUT_DIR is not set and exported in\n"
                                + "\tyour COMPILE.SH?!" );
            System.out.println( "Usually those should be set automatically by BenchIt." );
            System.out.println( "This is quite an unusual error!! Did you create your LOCALDEFS??" );
            System.exit( 2 );
        }
        try {
            c = Class.forName( kernelName );
        } catch( ClassNotFoundException cnfe ) {
            System.err.println( "The Java-BenchIt-Kernel-class was not found: "
                                + kernelName );
            System.err.println( "Make sure to have a " + kernelName
                                + ".java in your kernel directory!" );
            System.exit( 1 );
        }
        if( c != null ) {
            try {
                kernel = ( BIJavaKernel ) ( c.newInstance() );
            } catch( ClassCastException cce ) {
                System.err.println( "Your " + kernelName + " class does not "
                                    + "implement the BIJavaKernel interface!" );
                System.exit( 1 );
            } catch( InstantiationException ie ) {
                System.err.println( "Your " + kernelName + " class could not "
                                    + "be instantiated!" );
                System.exit( 1 );
            } catch( IllegalAccessException iae ) {
                kernel = null;
            }
        }
        if( kernel == null ) {
            System.err.println( "For some unknown reason your " + kernelName + " class was not loaded!" );
            System.exit( 1 );
        } else {
            initialize( kernel );
            benchit();
        }
    }

    /** Prints list and explanation of command line args on stdout
     * and exits the program. */
    private void printHelpAndExit() {
        System.out.println( "Usage: RUN.SH " + rwe.bi_getEnv( "BENCHIT_KERNELNAME" )
                            + " [-options]\n" );
        System.out.println( "where options include:" );
        System.out.print( " -h,\t--help\t\t\t\t" );
        System.out.println( "prints this help screen" );
        System.out.print( " -d,\t--dumpTable\t\t\t" );
        System.out.println( "prints the environment variables stored in the internal\n"
                            + "\t\t\t\t\tHashTable to stdout" );
        System.out.print( " -D,\t--define=SWITCH[=Value]\t\t" );
        System.out.println( "sets control switches of BenchIT which are:\n"
                            + "\t\t\t\t\tNO_TIMELIMIT - ignores the time limit set in LOCALDEFS\n"
                            + "\t\t\t\t\tERROR_CORRECTION=n - repeats measurement n times\n"
                            + "\t\t\t\t\tDEBUGLEVEL=n - the higher n, the more debug messages\n"
                            + "\t\t\t\t\tLINEAR_MEASUREMENT - if set, problemsize increases 1 by 1" );
        System.out.print( " -o,\t--output_dir=DIR\t\t" );
        System.out.println( "writes result files into DIR" );
        System.out.print( " -p,\t--parameter_file=PAR_FILE\t" );
        System.out.println( "reads parameters from PAR_FILE at runtime" );
        System.out.print( " -q,\t--quiet\t\t\t\t" );
        System.out.println( "suppresses all messages to stdout and stderr" );
        System.out.print( " -v,\t--verbose\t\t\t" );
        System.out.println( "prints more messages about what the program is doing" );
        System.out.print( " -V,\t--version\t\t\t" );
        System.out.println( "prints version information" );

        System.out.flush();
        System.exit( 0 );
    }

    /* Return value of next method. */
    class ArgResult {
        int retval = 0;
        int pos = -1;
        String value = null;
        String valuesValue = null;
    };
    /** Checks if at position pos in args is an option in whether
     * short or long version. If hasValue is greater than 0 the
     * ArgResult's value will be set to the value string
     * and the varibale pos will be increased.
     * If args[i] is a short option the value is expected in args[i+1].
     * If args[i] is a long option, the value is that part of args[i]
     * that follows the first occuring = sign.
     * Also a value may have a value. The delimiter sign must be the = sign.
     * @param args the args String array.
     * @param res the ArgResult object used as return value.
     * @param sOpt the short version of the option.
     * @param lOpt the long version of the option.
     * @param hasValue 0 if option without value, 1 if with value.
     * @return modified res if successful match and retrieval of value (retval=1), (retval=0) else.*/
    private ArgResult isOption( String[] args, ArgResult res, char sOpt, String lOpt, int hasValue ) {
        int len = -1;
        res.retval = 0;
        res.value = null;
        res.valuesValue = null;
        if( res.pos >= args.length ) {
            return res;
        }
        len = args[res.pos].length();
        if( args[res.pos].charAt( 0 ) != '-' ) {
            return res;
        }
        /* now try to match option */
        if( len == 2 ) {
            /* short option */
            if( sOpt == 0 ) {
                return res;
            }
            if( hasValue > 0 && res.pos + 1 >= args.length ) {
                return res;
            }
            if( args[res.pos].charAt( 1 ) == sOpt ) {
                /* short option hit */
                int ep = -1;
                if( hasValue == 1 ) {
                    res.value = args[++res.pos];
                    ep = res.value.indexOf( "=", 0 );
                }
                res.retval = 1;
                if( ep >= 0 ) {
                    res.valuesValue = res.value.substring( ep + 1 );
                    res.value = res.value.substring( 0, ep );
                }
            } else {
                return res;
            }
        } else if( len > 2 && lOpt != null ) {
            /* long option */
            String sub;
            int eqPos = -1;
            if( args[res.pos].charAt( 1 ) != '-' ) {
                return res;
            }
            if( hasValue == 1 ) {
                eqPos = args[res.pos].indexOf( "=", 1 );
            } else {
                eqPos = len + 1;
            }
            if( eqPos < 3 ) {
                return res;
            }
            if( eqPos >= args[res.pos].length() ) {
                return res;
            }
            /* extract option name */
            sub = args[res.pos].substring( 2, eqPos );
            if( sub == null ) {
                return res;
            }
            if( sub.compareTo( lOpt ) == 0 ) {
                /* long option hit */
                int ep = -1;
                if( hasValue == 1 ) {
                    res.value = args[res.pos].substring( eqPos + 1 );
                    ep = res.value.indexOf( "=", 0 );
                }
                res.retval = 1;
                if( ep >= 0 ) {
                    res.valuesValue = res.value.substring( ep + 1 );
                    res.value = res.value.substring( 0, ep );
                }
            }
        } else {
            return res;
        }
        return res;
    }

    /** Checks the arguments for recognizable input. Does not use
     * getopt for compatibility reasons. See documentation of
     * printUsage for detailed info about recognized options. */
    public void checkCommandLine( String[] args ) {
        rwe.bi_put( "BENCHIT_PARAMETER_EXECUTABLE_NAME", rwe.bi_getEnv( "BENCHIT_KERNELNAME" ) + "." + rwe.bi_getEnv( "BENCHIT_FILENAME_COMMENT" ) + File.separator + "RUN.SH" );
        int i = 0;
        ArgResult res = new ArgResult();
        for( i = 0; i < args.length; i++ ) {
            res.pos = i;
            res = isOption( args, res, 'h', "help", 0 );
            if( res.retval == 1 ) {
                printHelpAndExit();
                continue;
            }
            res = isOption( args, res, 'd', "dumpTable", 0 );
            if( res.retval == 1 ) {
                rwe.bi_dumpTable();
                System.out.flush();
                System.exit( 0 );
                continue;
            }
            res = isOption( args, res, 'D', "define", 1 );
            if( res.retval == 1 ) {
                if( res.value.compareTo( "BENCHIT_RUN_ACCURACY" ) == 0 ) {
                    if( res.valuesValue != null ) {
                        try {
                            ERROR_CORRECTION = ( new Integer( res.valuesValue ) ).intValue() + 1;
                        } catch( NumberFormatException exc ) {
                            System.err.println( "Theres a problem with your ERROR_CORRECTION-setting!" );
                            System.exit( 1 );
                        }
                    }
                } else if( res.value.compareTo( "BENCHIT_DEBUGLEVEL" ) == 0 ) {
                    if( res.valuesValue != null ) {
                        try {
                            debugLevel = ( new Integer( res.valuesValue ) ).intValue();
                        } catch( NumberFormatException exc ) {
                            System.err.println( "Theres a problem with your DEBUGLEVEL-setting" );
                            System.exit( 1 );
                        }
                    }
                } else if( res.value.compareTo( "BENCHIT_RUN_LINEAR" ) == 0 ) {
                    linearMeasurement = true;
                } else if( res.value.compareTo( "NO_TIMELIMIT" ) == 0 ) {
                    timelimit = false;
                } else if( res.value.compareTo( "VAMPIR_TRACE" ) == 0 ) {
                    vampireTrace = true;
                }
                if( res.valuesValue == null ) {
                    rwe.bi_put( res.value, "" );
                } else {
                    rwe.bi_put( res.value, res.valuesValue );
                }
                i = res.pos;
                continue;
            }
            res = isOption( args, res, 'o', "output-dir", 1 );
            if( res.retval == 1 ) {
                rwe.bi_put( "BENCHIT_RUN_OUTPUT_DIR", res.value );
                outputDir = res.value;
                i = res.pos;
                continue;
            }
            res = isOption( args, res, 'p', "parameter-file", 1 );
            if( res.retval == 1 ) {
                rwe.bi_put( "BENCHIT_PARAMETER_FILE", res.value );
                rwe.bi_readParameterFile( res.value );
                i = res.pos;
                continue;
            }
            res = isOption( args, res, 'q', "quiet", 0 );
            if( res.retval == 1 ) {
                /* Redirection of stdout and stderr. */
                // Set up System.out
                try {
                    PipedInputStream piOut = new PipedInputStream();
                    PipedOutputStream poOut = new PipedOutputStream( piOut );
                    System.setOut( new PrintStream( poOut, true ) );
                    // Set up System.err
                    PipedInputStream piErr = new PipedInputStream();
                    PipedOutputStream poErr = new PipedOutputStream( piErr );
                    System.setErr( new PrintStream( poErr, true ) );
                } catch( IOException ioe ) {
                    System.out.println( "Quiet mode not possible. Error while redirecting output and error streams." );
                }
            }
            res = isOption( args, res, 'v', "verbose", 0 );
            if( res.retval == 1 ) {
                verbose = true;
                continue;
            }
            res = isOption( args, res, 'V', "version", 0 );
            if( res.retval == 1 ) {
                System.out.println( "BenchIT version 2006.05.09\n" );
                System.out.flush();
                System.exit( 0 );
                continue;
            }
            /* if this point of the loop is reached, the argument
               is not a recognized option */
            System.err.println( "BenchIT: Unknown argument: " + args[i] );
            System.exit( 1 );
        }
    }

    public void benchit() {
        Properties properties = System.getProperties();
        Enumeration propenum = properties.propertyNames();
        /* ############################################################## */
        InfoObject info = new InfoObject();
        StringBuffer outputBuffer = new StringBuffer();
        StringBuffer quickviewBuffer = new StringBuffer();
        System.out.print( "BenchIT: Getting info about kernel..." );
        if( kernel.bi_getinfo( info ) != 0 ) {
            System.out.println( "[  FAILED  ]" );
            System.out.flush();
            System.exit( 1 );
        }
        if( info.yaxistexts == null || info.legendtexts == null ) {
            System.out.println( "[  FAILED  ]" );
            System.out.println( "Kernel doesn't set yaxistexts or legendtexts!" );
            System.out.flush();
            System.exit( 1 );
        }
        /* get kernel language and libraries from kernel name */
        getKernelInfos();
        ydata = new AxisData[info.numfunctions];
        for( int i = 0; i < info.numfunctions; ++i ) {
            ydata[i] = new AxisData( "y" );
        }
        info.outlier_direction_upwards = new int[info.numfunctions];
        info.log_yaxis = new int[info.numfunctions];
        info.base_yaxis = new int[info.numfunctions];
        System.out.print( "   [  OK  ]\nBenchIT: Getting start time..." );
        Date te = null;
        Date ts = new Date();
        System.out.println( "   [  OK  ]\nBenchIT: Selected kernel: " + kernelstring );
        System.out.print( "BenchIT: Initializing kernel..." );
        Object dObject = kernel.bi_init( info.maxproblemsize );
        int offset = info.numfunctions + 1;
        double[] tempresults = new double[offset];
        String filename = "LOCALDEFS" + File.separator + nname;
        String timelimitstring = rwe.bi_getEnv( "BENCHIT_RUN_TIMELIMIT" );
        if( timelimitstring == null ) {
            System.out.println( "   [  FAILED  ]" );
            System.out.println( "Check your LOCALDEFS! BENCHIT_RUN_TIMELIMIT is not set!" );
            System.exit( 1 );
        }
        // if there is no timelimit
        long timelimit = Long.MAX_VALUE;
        // if there is a time limit
        if( this.timelimit ) {
            try {
                timelimit = ( new Long( timelimitstring ) ).longValue();
            } catch( NumberFormatException nfe ) {
                System.err.println( "Couldn't parse timelimit-string " + timelimitstring );
                System.exit( -1 );
            }
        }
        System.out.print( "   [  OK  ]\nBenchIT: Allocating memory for results..." );
        allresults = new double[offset * info.maxproblemsize];
        this.m_nextProblemDone = new boolean[info.maxproblemsize + 1];
        this.m_nextProblemTODO = new int[info.maxproblemsize + 2];
        this.m_nextProblemMax = info.maxproblemsize;
        System.out.println( "   [  OK  ]\nBenchIT: Measuring..." );
        // for all problemsizes

        int v = 1, w = 1, n = 0, /*rank=0*/ flag = 0, m = 0;
        double time1, time2;
        // while there are problems
        while( this.get_new_problems() ) {
            v = 0;
            // we do those
            while( this.m_nextProblemTODO[++v] != 0 ) {
                // displacement of the subresult vector within allresults
                int problemOffset = offset * ( m_nextProblemTODO[v] - 1 );
                // initialize tempresults
                for( int i = 1; i < offset; i++ ) {
                    tempresults[i] = INVALID_MEASUREMENT;
                }
                // getting n
                n = ( ( ( int )Math.pow( 2, m_nextProblemTODO[0] - 1 ) ) + v - 1 );
                // debug
                IDL( 2, "Testing with problem size " + m_nextProblemTODO[v] );
                // we test ERROR_CORRECTION times
                for( w = -1; w < this.ERROR_CORRECTION; w++ ) {
                    //getting the time
                    time1 = bi_gettime();
                    //MPI no MPI
                    //MPI  if (rank == 0)
                    // flag says whether the operation was succesfull (1) or not (0)
                    // i hope i understood the code right, ask guido: juckeland@zhr.tu-dresden.de
                    // send him mails about commenting code!!!!!!!!!!!!!
                    // the last value in the method i think is the allresults(offset*(m_nextProblemTODO[v]-1))

                    // call the kernel here
                    double tempd[] = new double[offset];
                    flag = kernel.bi_entry( dObject, m_nextProblemTODO[v], tempd );
                    for( int i = 0; i < offset; i++ ) {
                        double d_val = tempd[i];
                        if( Double.isInfinite( d_val ) || Double.isNaN( d_val ) ) {
                            tempd[i] = INVALID_MEASUREMENT;
                        }
                        allresults[problemOffset + i] = tempd[i];
                    }

                    //MPI AGAIN MPI
                    //MPI  else if (theinfo.kernel_execs_mpi1 != 0)
                    //MPI    flag=bi_entry(mcb,v+1,0);
                    // if the operation didnt finish succesfully
                    if( flag != 0 ) {
                        //MPI And we are rank 0 in MPI, so forget it
                        //MPI if (rank==0)
                        System.out.print( "   [  FAILED  ]\nBenchIT: Internal kernel error. " );
                        //freeall(&theinfo,allresults);
                        System.exit( 1 );
                    }
                    // otherwise this problemsize has been succesfully measured
                    else {
                        this.m_nextProblemDone[m_nextProblemTODO[v]] = true;
                    }
                    // if we are RANK 0 (MPI)
//MPI          if (rank==0) {
                    // [0] is always the x axis
                    tempresults[0] = allresults[problemOffset];
                    // extract the "better" result value for each measured function
                    for( int i = 1; i < offset; i++ ) {
                        int index = problemOffset + i;
                        // initialize at the first measurement
                        if( w == -1 ) {
                            tempresults[i] = allresults[index];
                        }
                        // if allresults is a valid value
                        if( allresults[index] != INVALID_MEASUREMENT ) {
                            // get the better result according to outlier direction
                            // information from the info object
                            if( info.outlier_direction_upwards[i - 1] > 0 ) {
                                if( tempresults[i] > allresults[index] ) {
                                    tempresults[i] = allresults[index];
                                }
                                if( tempresults[i] == INVALID_MEASUREMENT ) {
                                    // if tempresults is invalid replace it in any case
                                    tempresults[i] = allresults[index];
                                }
                            }
                            if( ( info.outlier_direction_upwards[i - 1] ) < 1 ) {
                                if( tempresults[i] < allresults[index] ) {
                                    tempresults[i] = allresults[index];
                                }
                            }
                        }
                    }
//MPI                  }
                    IDL( 3, "tempresults=" );
                    for( int i = 0; i < offset; i++ ) {
                        if( this.debugLevel >= 3 ) {
                            System.out.print( tempresults[i] );
                        }
                    }
                    IDL( 3, "\nallresults[" + ( m_nextProblemTODO[v] - 1 ) + "]=" );
                    for( int i = 0; i < offset; i++ ) {
                        if( this.debugLevel >= 3 ) {
                            System.out.print( allresults[offset * ( m_nextProblemTODO[v] - 1 ) + i] );
                        }
                    }
                    IDL( 3, "" );
                }
                // if rank == 0
                // copy back extracted better values to allresults
                for( int i = 0; i < offset; i++ ) {
                    allresults[problemOffset + i] = tempresults[i];
                }
                m++;
                if( 50 * m / info.maxproblemsize != 50 * ( m + 1 ) / info.maxproblemsize ) {
                    System.out.print( "." );
                    System.out.flush();
                }
                te = new Date();
                if( te == null ) {
                    System.out.print( "\nBenchIT: Time limit measurement error. Stopping  measurement." );
                    break;
                }
                if( te.getTime() - ts.getTime() > timelimit * 1000 ) {
                    System.out.print( "\nBenchIT: Total time limit reached. Stopping measurement." );
                    break;
                }
            }
            te = new Date();
            if( te == null ) {
                System.out.print( "\nBenchIT: Time limit measurement error. Stopping    measurement." );
                break;
            }
            if( te.getTime() - ts.getTime() > timelimit * 1000 ) {
                System.out.print( "\nBenchIT: Total time limit reached. Stopping measurement." );
                break;
            }
        }
        IDL( 2, "...OK" );

        System.out.print( "\nBenchIT: Analyzing results..." );
        // this flag is used for delayed initialization in case the first n
        // results were not measured
        boolean initialized = false;
        for( int i = 0; i < info.maxproblemsize; i++ ) {
            // ignore result values which were not measured
            if( allresults[i * offset] == 0.0 ) {
                continue;
            }
            // initialize min and max values on first
            if( ( i == 0 ) || ( !initialized ) ) {
                initialized = true;
                xdata.min = allresults[i];
                xdata.max = xdata.min;
                for( int j = 1; j < offset; j++ ) {
                    ydata[j - 1].min = allresults[i * offset + j];
                    ydata[j - 1].max = allresults[i * offset + j];
                }
            }
            // first look for min and max x values
            if( ( allresults[i * offset] < xdata.min ) && ( allresults[i * offset] != INVALID_MEASUREMENT ) ) {
                xdata.min = allresults[i * offset];
            } else if( xdata.min == INVALID_MEASUREMENT ) {
                xdata.min = allresults[i * offset];
            }
            if( allresults[i * offset] > xdata.max ) {
                xdata.max = allresults[i * offset];
            }
            // now look for min and max y values
            for( int j = 1; j < offset; j++ ) {
                if( ( allresults[i * offset + j] < ydata[j - 1].min ) && ( allresults[i * offset] != INVALID_MEASUREMENT ) ) {
                    ydata[j - 1].min = allresults[i * offset + j];
                }
                // no matter what if the current ymin value is INVALID_MEASUREMENT
                // it must be replaced by the current result value
                else if( ydata[j - 1].min == INVALID_MEASUREMENT ) {
                    ydata[j - 1].min = allresults[i * offset + j];
                }
                if( allresults[i * offset + j] > ydata[j - 1].max ) {
                    ydata[j - 1].max = allresults[i * offset + j];
                }
            }
        }
        /* Get global minimum and maximum for y-axis */
        ydata_global.min = 1e30;
        ydata_global.max = -1e30;
        for( int i = 0; i < info.numfunctions; ++i ) {
            if( ydata[i].min < ydata_global.min ) {
                ydata_global.min = ydata[i].min;
            }
            if( ydata[i].max > ydata_global.max ) {
                ydata_global.max = ydata[i].max;
            }
        }
        System.out.print( "   [  OK  ]\nBenchIT: Writing resultfile..." );
        rwd = new RWDate();
        fillOutputBuffer( outputBuffer, info );
        filename = "LOCALDEFS" + File.separator + nname;
        File file = new File( filename );
        if( !file.exists() ) {
            filename = "LOCALDEFS" + File.separator + "PROTOTYPE";
        }
        String archShort = rwe.bi_getEnv( "BENCHIT_ARCH_SHORT" );
        String archSpd = rwe.bi_getEnv( "BENCHIT_ARCH_SPEED" );
        if( ( archShort == null ) || ( archSpd == null ) ) {
            System.out.println( "   [  FAILED  ]" );
            System.out.println( "Check your LOCALDEFS! At least one of BENCHIT_ARCH_SHORT, BENCHIT_ARCH_SPEED is not set!" );
            System.exit( 1 );
        }
        String time = rwd.formatDate( "%Y_%mm_%dd__#HH_#mm" );
        String fname; /*eg.: InP3_700M__v01__2005_07_14__13_51.bit*/
        String kernelComment = rwe.bi_getEnv( "BENCHIT_FILENAME_COMMENT" );
        /* kernelstring is the kernel name, eg.: numerical.matmul.Java.0.0.double */
        String outDir = kernelstring;
        outDir = outDir.replace( '.', File.separatorChar );
        /*eg.: numerical/matmul/Java/0/0/double*/
        fname = archShort + "_" + archSpd + "__" + kernelComment + "__" + time + ".bit";
        StringBuffer sb = new StringBuffer( fname );
        for( int i = 0; i < sb.length(); i++ ) {
            /* This is neccessary, because on IRIX the JVM extracts the '\"' char out of property files, too.*/
            if( sb.charAt( i ) == '\"' ) {
                sb = sb.deleteCharAt( i );
            }
        }
        fname = sb.toString();
        filename = outputDir + File.separator + outDir + File.separator;
        /* check if path exists, if not create it */
        file = new File( filename );
        if( !file.exists() ) {
            try {
                file.mkdirs();
            } catch( SecurityException se ) {
                System.out.println( "   [  FAILED  ]\nBenchIT: could not create output directory:\n" + filename );
                System.out.println( "A SecurityManager doesn't permit creating the directory!" );
                System.exit( 1 );
            }
        } else if( !file.isDirectory() ) {
            System.out.println( "   [  FAILED  ]\nBenchIT: Output directory path is not a directory:\n" + filename );
            System.exit( 1 );
        }
        String pathname = filename;
        filename = pathname + fname;
        saveOutputBuffer( outputBuffer, filename );
        System.out.println( "   [  OK  ]\nBenchIT: Wrote output to \"" + fname + "\" in directory\n\"" + pathname + "\"" );
        System.out.print( "BenchIT: Writing quickview file..." );
        filename = filename + ".gp";
        fillQuickviewBuffer( quickviewBuffer, info, fname );
        saveOutputBuffer( quickviewBuffer, filename );
        System.out.println( "   [  OK  ]\nBenchIT: Finishing...   [  OK  ]" );
        /* ############################################################## */
    }

    private void saveOutputBuffer( StringBuffer outputBuffer, String filename ) {
        FileWriter out;
        try {
            out = new FileWriter( filename );
            out.write( outputBuffer.toString() );
            out.close();
        } catch( IOException ioe ) {
        }
    }

    private void fillQuickviewBuffer( StringBuffer quickviewBuffer, InfoObject info, String fname ) {
        quickviewBuffer.append( "#gnuplotfile\n" );
        String temp = "";
        if( kernelstring != null ) {
            temp = kernelstring;
        }
        quickviewBuffer.append( "set title \"" + temp + "\"\n" );
        temp = "";
        if( info.xaxistext != null ) {
            temp = info.xaxistext;
        }
        quickviewBuffer.append( "set xlabel \"" + temp + "\"\n" );
        temp = "";
        if( info.yaxistexts != null ) {
            temp = info.yaxistexts[0];
        }
        quickviewBuffer.append( "set ylabel \"" + temp + "\"\n" );
        quickviewBuffer.append( "set data style points\n" );
        quickviewBuffer.append( "set term postscript eps color\n" );
        quickviewBuffer.append( "set output \"" + fname + ".gp.eps\"\n" );
        xdata.computeAxisProperties();
        fillGnuplotAxisProperties( quickviewBuffer, xdata );
        ydata_global.computeAxisProperties();
        fillGnuplotAxisProperties( quickviewBuffer, ydata_global );

        for( int i = 0; i < info.numfunctions; i++ ) {
            if( i != 0 ) {
                quickviewBuffer.append( "," );
            } else {
                quickviewBuffer.append( "plot" );
            }
            temp = "";
            if( info.legendtexts[i] != null ) {
                temp = info.legendtexts[i];
            }
            quickviewBuffer.append( " \"" + fname + "\" using 1:" + ( i + 2 ) + " title '" + temp + "'" );
        }
        quickviewBuffer.append( "\n" );
    }

    private void fillGnuplotAxisProperties( StringBuffer buf, AxisData ad ) {
        double pos, pos1;
        int i;
        boolean have_scaling = false, first = true;
        ScalingVars sv = new ScalingVars();
        java.text.DecimalFormatSymbols dfs = new java.text.DecimalFormatSymbols();
        dfs.setDecimalSeparator( '.' );
        dfs.setGroupingSeparator( ',' );
        java.text.DecimalFormat nf = new java.text.DecimalFormat( "######0.####", dfs );
        if (ad.incr==0.0)
          ad.incr=1.0;
        switch( ( int )ad.base ) {
        case 0: { // linear scale
            buf.append( "set " + ad.name + "tics (" );
            if( ( ad.plotmin < 0 ) && ( ad.plotmax > 0 ) ) { // search for value nearest to 0 for determining the scaling
                double dist = Double.POSITIVE_INFINITY;
                for( pos = ad.plotmin; pos <= ad.plotmax; pos += ad.incr ) {
                    if( Math.abs( pos ) < dist ) {
                        dist = pos;
                    }
                }
                getScaling( dist, sv, false );
                have_scaling = true;
            }
            for( pos = ad.plotmin; pos <= ad.plotmax; pos += ad.incr ) {
                //print_axisdata( ad );
                if( !have_scaling ) {
                    getScaling( pos, sv, false );
                    if( pos != 0.0 ) {
                        have_scaling = true;
                    }
                }
                pos1 = pos * sv.scalingValue;
                if( first ) {
                    buf.append( "\"" );
                    nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                    buf.append( SI_prefixes[sv.scalingLevel + SI_PREF_ZERO] + "\" " + new Double( pos ) );
                    first = false;
                } else {
                    buf.append( ",\"" );
                    nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                    buf.append( SI_prefixes[sv.scalingLevel + SI_PREF_ZERO] + "\" " + new Double( pos ) );
                }
            }
            buf.append( ")\n" );
            break;
        }
        case 2: {
        System.out.print(4);
            buf.append( "set " + ad.name + "tics (" );
            for( pos = ad.plotmin; pos <= ad.plotmax; pos *= ad.incr ) {
                if( !have_scaling ) {
                    getScaling( pos, sv, true );
                    if( pos != 0.0 ) {
                        have_scaling = true;
                    }
                }
                pos1 = pos * sv.scalingValue;
                if( first ) {
                    buf.append( "\"" );
                    nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                    buf.append( BIN_pref[sv.scalingLevel] + "\" " + new Double( pos ) );
                    first = false;
                } else {
                    buf.append( ",\"" );
                }
                nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                buf.append( BIN_pref[sv.scalingLevel] + "\" " + new Double( pos ) );
            }
            buf.append( ")\n" );
            buf.append( "set logscale " + ad.name + " " + new Integer( ( int )ad.base ) + "\n" );
            break;
        }
        case 10: {
        System.out.print(5);
            buf.append( "set " + ad.name + "tics (" );
            for( pos = ad.plotmin; pos <= ad.plotmax; pos *= ad.incr ) {
                /*
                 if( ad.ticks <= 3 )
                 { // use single scaling only if few ticks
                      if( !have_scaling )
                      {
                 getScaling( pos, sv, 0 );
                 if( pos != 0.0 )
                 have_scaling = true;
                      }
                 }
                 else
                 */
                getScaling( pos, sv, false );
                pos1 = pos * sv.scalingValue;
                if( first ) {
                    buf.append( "\"" );
                    nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                    buf.append( SI_prefixes[sv.scalingLevel + SI_PREF_ZERO] + "\" " + new Double( pos ) );
                    first = false;
                } else {
                    buf.append( ",\"" );
                    nf.format( pos1, buf, new FieldPosition( NumberFormat.INTEGER_FIELD ) );
                    buf.append( SI_prefixes[sv.scalingLevel + SI_PREF_ZERO] + "\" " + new Double( pos ) );
                }
                if( ad.ticks <= 5 ) {
                    for( i = 2; i <= 9; ++i ) {
                        buf.append( ",\"\" " + new Double( pos * i ) + " 1" );
                    }
                }
            }
            buf.append( ")\n" );
            buf.append( "set logscale " + ad.name + " " + new Integer( ( int )ad.base ) + "\n" );
            break;
        }
        default:
            buf.append( "set " + ad.name + "tics " + new Double( ad.plotmin ) + "," + new Double( ad.incr ) + "\n" );
            break;
        }
        buf.append( "set " + ad.name + "range [" + new Double( ad.plotmin ) + ":" + new Double( ad.plotmax ) + "]\n" );
    }

    private void fillOutputBuffer( StringBuffer outputBuffer, InfoObject info ) {
        int i = 0;
        outputBuffer.append( "# BenchIT-Resultfile\n#\n" );
        outputBuffer.append( "# feel free to fill in architectural information\n" );
        outputBuffer.append( "################################################\n" );
        outputBuffer.append( "beginofmeasurementinfos\n" );
        outputBuffer.append( "kernelstring=\"" + kernelstring + "\"\n" );
        outputBuffer.append( "date=\"" + rwd.formatDate( "%M %dd #HH:#mm:#ss %Y" ) + "\"\n" );
        outputBuffer.append( "numberofprocessorsused=\"" + rwe.bi_getEnv( "BENCHIT_NUM_PROCESSES" ) + "\"\n" );
        outputBuffer.append( "memorysizeused=\"" + rwe.bi_getEnv( "BENCHIT_RUN_MAX_MEMORY" ) + "\"\n" );
        outputBuffer.append( "comment=\"" + rwe.bi_getEnv( "BENCHIT_KERNEL_COMMENT" ) + "\"\n" );
        outputBuffer.append( "language=\"Java\"\n" );
        outputBuffer.append( "compiler=\"" + rwe.bi_getEnv( "BENCHIT_COMPILER" ) + "\"\n" );
        outputBuffer.append( "compilerflags=\"" + rwe.bi_getEnv( "BENCHIT_COMPILERFLAGS" ) + "\"\n" );
        for( i = 0; i < libraries.length; i++ ) {
            outputBuffer.append( "library" + ( i + 1 ) + "=\"" + libraries[i] + "\"\n" );
        }
        i = 0;
        i += info.kernel_execs_mpi1;
        i += info.kernel_execs_mpi2;
        i += info.kernel_execs_pvm;
        i += info.kernel_execs_omp;
        i += info.kernel_execs_pthreads;
        i += info.kernel_execs_javathreads;
        if( i == 0 ) {
            outputBuffer.append( "kernelisparallel=0\n" );
        } else {
            outputBuffer.append( "kernelisparallel=1\n" );
        }
        outputBuffer.append( "codesequence=\"" + info.codesequence + "\"\n" );
        outputBuffer.append( "xinmin=" + xdata.min + "\n" );
        outputBuffer.append( "xinmax=" + xdata.max + "\n" );
        for( int j = 0; j < info.numfunctions; j++ ) {
            outputBuffer.append( "yinmin=" + ydata[j].min + "\n" );
        }
        for( int j = 0; j < info.numfunctions; j++ ) {
            outputBuffer.append( "yinmax=" + ydata[j].max + "\n" );
        }
        outputBuffer.append( "endofmeasurementinfos\n################################################\n" );
        outputBuffer.append( "beginofarchitecture\n# Architekturangaben\n" );
        outputBuffer.append( "nodename=\"" + rwe.bi_getEnv( "BENCHIT_NODENAME" ) + "\"\n" );
        outputBuffer.append( "hostname=\"" + rwe.bi_getEnv( "BENCHIT_HOSTNAME" ) + "\"\n" );
        String fname = rwe.bi_getEnv( "BENCHITROOT" ) + File.separator + "LOCALDEFS" + File.separator + nname + "_input_architecture";
        File file = new File( fname );
        if( !file.exists() ) {
            fname = rwe.bi_getEnv( "BENCHITROOT" ) + File.separator + "LOCALDEFS" + File.separator + "PROTOTYPE" + "_input_architecture";
        }
        appendFileContent( fname, outputBuffer );
        /* save the environment variables used during execution */
        rwe.dumpToOutputBuffer( outputBuffer );
        appendFileContent( fname, outputBuffer );
        outputBuffer.append( "beginofdisplay\n" );
        /* first the x-axis */
        xdata.computeAxisProperties();
        outputBuffer.append( "\nxoutmin=" + xdata.plotmin + "\n" );
        outputBuffer.append( "xoutmax=" + xdata.plotmax + "\n" );
        outputBuffer.append( "xaxisticks=" + xdata.ticks + "\n" );
        outputBuffer.append( "xaxislogbase=" + xdata.base + "\n" );
        if( info.xaxistext == null ) {
            outputBuffer.append( "xaxistext=\n" );
        } else {
            outputBuffer.append( "xaxistext=\"" + info.xaxistext + "\"\n" );
        }
        /* now the y-axises */
        for( int j = 0; j < info.numfunctions; j++ ) {
            ydata[j].computeAxisProperties();
            outputBuffer.append( "\ny" + ( j + 1 ) + "outmin=" + ydata[j].plotmin + "\n" );
            outputBuffer.append( "y" + ( j + 1 ) + "outmax=" + ydata[j].plotmax + "\n" );
            outputBuffer.append( "y" + ( j + 1 ) + "axisticks=" + ydata[j].ticks + "\n" );
            outputBuffer.append( "y" + ( j + 1 ) + "axislogbase=" + ydata[j].base + "\n\n" );
            /* write axis texts */
            if( info.yaxistexts[j] == null ) {
                outputBuffer.append( "y" + ( j + 1 ) + "axistext=\n" );
            } else {
                outputBuffer.append( "y" + ( j + 1 ) + "axistext=\"" + info.yaxistexts[j] + "\"\n" );
            }
        }
        outputBuffer.append( "\nnumabscissae=" + numabscissae + "\n" );
        outputBuffer.append( "numfunctions=" + info.numfunctions + "\n\n" );
        outputBuffer.append( "## Fuer Architektur- oder Messmerkmal : abc\n#\n" );
        outputBuffer.append( "# displayabc=1                                           # Steuervariable\n" );
        outputBuffer.append( "# #Textfeld-Eigenschaften des date-Strings               # Kommentar\n" );
        outputBuffer.append( "# tabc=\"ABC-Merkmal: 3 Tm\"                               # Text: Wert\n" );
        outputBuffer.append( "# xabc=                                                  # x-Position\n" );
        outputBuffer.append( "# yabc=                                                  # y-Position\n" );
        outputBuffer.append( "# fonttypabc=                                            # Schriftart\n" );
        outputBuffer.append( "# fontsizeabc=                                           # Schriftgroesse\n" );
        for( i = 0; i < info.numfunctions; i++ ) {
            if( i == 0 ) {
                if( info.legendtexts == null ) {
                    System.err.println( "BenchIT: info->legendtexts==null\n" );
                    System.err.flush();
                    System.exit( 127 );
                }
            }
            outputBuffer.append( "tlegendfunction" + ( i + 1 ) + "=\"" + info.legendtexts[i] + "\"\n" );
            outputBuffer.append( "xlegendfunction" + ( i + 1 ) + "=\n" );
            outputBuffer.append( "ylegendfunction" + ( i + 1 ) + "=\n" );
            outputBuffer.append( "fonttypelegendfunction" + ( i + 1 ) + "=\n" );
            outputBuffer.append( "fontsizelegendfunction" + ( i + 1 ) + "=\n\n" );
        }
        fname = rwe.bi_getEnv( "BENCHITROOT" ) + File.separator + "LOCALDEFS" + File.separator + nname + "_input_display";
        file = new File( fname );
        if( !file.exists() ) {
            fname = rwe.bi_getEnv( "BENCHITROOT" ) + File.separator + "LOCALDEFS" + File.separator + "PROTOTYPE" + "_input_display";
        }
        appendFileContent( fname, outputBuffer );
        outputBuffer.append( "beginofdata\n" );
        for( i = 0; i < info.maxproblemsize; i++ ) {
            int offset = info.numfunctions + 1;
            // skip the results of a problemsize that was not meassured due to
            // the benchit time limit
            if( allresults[i * offset] == 0.0 ) {
                continue;
            }
            for( int j = 0; j < offset; j++ ) {
                double d_val = allresults[i * offset + j];
                if( d_val != INVALID_MEASUREMENT ) {
                    outputBuffer.append( d_val + "\t" );
                } else {
                    outputBuffer.append( "-\t" );
                }
            }
            outputBuffer.append( "\n" );
        }
        outputBuffer.append( "endofdata\n" );
    }

    private void appendFileContent( String filename, StringBuffer outputBuffer ) {
        File textFile = null;
        try {
            textFile = new File( filename );
        } catch( NullPointerException npe ) {
            System.err.println( "BenchIT: ERROR: Unable to open file for appending to buffer: " + filename );
        }
        if( textFile != null ) {
            if( textFile.exists() ) {
                FileReader in;
                char ch = '\0';
                int size = ( int ) ( textFile.length() );
                int read = 0;
                char inData[] = new char[size];
                try {
                    in = new FileReader( textFile );
                    in.read( inData, 0, size );
                    in.close();
                } catch( IOException ex ) {
                    System.err.println( "BenchIT: ERROR: Unable to append file to buffer: " + filename );
                }
                outputBuffer.append( new String( inData ) );
            } else {
                System.err.println( "BenchIT: ERROR: File doesn't exist: " + filename );
            }
        }
    }

    /** Retrieves source language and libraries from the kernel
     * name. The kernel name has be conform to BenchIT specifications:
     * www.benchit.org */
    private void getKernelInfos() {
        String kernelName = kernelstring;
        StringTokenizer st = new StringTokenizer( kernelName, "." );
        TreeSet libs = new TreeSet();
        int id = 0;
        language = "unknown";
        while( st.hasMoreTokens() ) {
            id++;
            String tok = st.nextToken();
            /* get 3rd category of kernel name: language */
            if( id == 3 ) {
                language = tok;
            }
            /* get 4th and 5th category of kernel name: par. libs, other libs */
            else if( id == 4 ) {
                StringTokenizer st2 = new StringTokenizer( tok, "--" );
                while( st2.hasMoreTokens() ) {
                    String tok2 = st2.nextToken();
                    if( tok2.compareTo( "0" ) != 0 ) {
                        libs.add( tok2 );
                    }
                }
            } else if( id == 5 ) {
                StringTokenizer st2 = new StringTokenizer( tok, "--" );
                while( st2.hasMoreTokens() ) {
                    String tok2 = st2.nextToken();
                    if( tok2.compareTo( "0" ) != 0 ) {
                        libs.add( tok2 );
                    }
                }
            }
        }
        libraries = new String[libs.size()];
        id = 0;
        Iterator it = libs.iterator();
        while( it.hasNext() ) {
            libraries[id++] = ( String )it.next();
        }
    }

    private void getScaling( double value, ScalingVars sv, boolean bBase2Scaling ) {
        double scalingBase = 1000.0;

        if( bBase2Scaling ) {
            scalingBase = 1024.0;
        }
        sv.scalingLevel = 0;
        sv.scalingValue = 1.0;
        if( value == 0 ) {
            return;
        }
        if( value < 0 ) {
            value = Math.abs( value );
        }
        if( value >= scalingBase ) {
            while( value >= scalingBase ) {
                value = ( float ) ( value / scalingBase );
                sv.scalingLevel += 1;
            }
        } else if( ( value < 1.0 ) && ( !bBase2Scaling ) ) { // "negative" scaling not possible for base 2
            while( value < 1.0 ) {
                value = ( float ) ( value * scalingBase );
                sv.scalingLevel -= 1;
            }
        }
        if( bBase2Scaling ) {
            if( sv.scalingLevel > BIN_PREF_MAX ) {
                sv.scalingLevel = BIN_PREF_MAX;
            }
        } else {
            if( sv.scalingLevel > SI_PREF_MAX ) {
                sv.scalingLevel = SI_PREF_MAX;
            } else if( sv.scalingLevel < SI_PREF_MIN ) {
                sv.scalingLevel = SI_PREF_MIN;
            }
        }
        sv.scalingValue = Math.pow( scalingBase, ( double ) ( sv.scalingLevel ) * ( -1 ) );
    }

    boolean get_new_problems() {
        int stepsize = ( int ) ( this.m_nextProblemMax / ( int )Math.pow( 2, ++this.m_nextProblemTODO[0] ) );
        int i = 0, k = 1, inc = 0;
        boolean ret = !this.m_nextProblemDone[0];

        IDL( 1, "Entering get_new_problems for " + this.m_nextProblemTODO[0] + ". time" );
        IDL( 2, "stepsize=" + stepsize );
        if( stepsize < ( 0.05 * this.m_nextProblemMax ) ) {
            stepsize = 1;
        }
        if( this.linearMeasurement ) {
            stepsize = 1;
        }
        if( stepsize == 1 ) {
            inc = 1;
        } else {
            inc = 2 * stepsize;
        }
        for( i = stepsize; i <= this.m_nextProblemMax; i += inc ) {
            if( !this.m_nextProblemDone[i] ) {
                this.m_nextProblemTODO[k++] = i;
            }
        }
        this.m_nextProblemTODO[k] = 0;
        if( stepsize == 1 ) {
            this.m_nextProblemDone[0] = true;
        } else if( this.m_nextProblemTODO[k - 1] == this.m_nextProblemMax ) {
            this.m_nextProblemTODO[k - 1] = 0; /*remove max from todo unless stepsize=1*/
        }
        IDL( 2, "todolist=" );
        for( i = 1; i <= this.m_nextProblemMax; i++ ) {
            IDL( 2, " " + this.m_nextProblemTODO[i] );
        }
        IDL( 2, "" );
        IDL( 1, "Leaving get_new_problems." );
        return( ret );
    }

    private void IDL( int minDebugLevel, String message ) {
        if( this.debugLevel >= minDebugLevel ) {
            System.err.println( message );
        }
    }

	/* !
	if you add new nativ methods you have to recreate the JBI.h:
	   javac JBI.java
	   javah JBI
	   cp JBI.h jni
	and provide an implementation of the function in jni/JBI.c
	*/

	/**
	* C-Timer-Function (see jni/JNI_Timer.c)
	* @return The elapsed time since 01.01.1970 in seconds.
	*/
	private native static double get_time_of_day();

	
	/**
	* @return The elapsed time since 01.01.1970 in seconds.
	*/
	public static double bi_gettimeofday()
	{
		if (jni==true)
		{	
			return get_time_of_day();
		}
		else
		{
			return (double) System.currentTimeMillis()/1000;
		}
		
	}


	/**
	* @return The elapsed time since 01.01.1970 in seconds.
	*/
	public static double bi_gettime()
	{
		return bi_gettimeofday();
	}
}

/******************************************************************************
 *  Log-History
 *
 *  $Log: JBI.java,v $
 *  Revision 1.22  2007/02/01 15:38:01  molka
 *  changed Java-Kernels to use C-Timer (JBI.bi_gettime()) instead of System.currentTimeMillis()
 *
 *  Revision 1.21  2006/12/07 16:33:41  molka
 *  fixed bug when compiling JNI-library
 *
 *  Revision 1.20  2006/12/07 15:03:50  molka
 *  Added C-Timer for Java-kernels
 *
 *  Revision 1.19  2006/05/11 11:16:12  hackenb
 *  *** empty log message ***
 *
 *  Revision 1.18  2006/05/09 12:56:41  rschoene
 *  problems with --output-dir
 *
 *  Revision 1.17  2006/05/09 12:12:03  rschoene
 *  some problems and bugs fixed
 *
 *  Revision 1.16  2006/05/09 11:17:03  rschoene
 *  axis base is now int
 *
 *  Revision 1.15  2006/01/02 15:30:13  mickler
 *  # New environment variable names
 *
 *  Revision 1.14  2005/11/12 10:09:17  mickler
 *  + New .gp file generation and axis properties computation as in benchit.c
 *
 *  Revision 1.13  2005/10/26 14:14:21  mickler
 *  # sanity check for get_{x,y}axis_properties()
 *
 *  Revision 1.12  2005/10/25 11:55:49  mickler
 *  - Fixed linear ticks calculation
 *  - Fixed search for LOCALDEFS (nname instead of hname must be used)
 *  - Removed deprecated output file entries
 *
 *  Revision 1.11  2005/09/11 22:48:52  mickler
 *  - Fixed: wrong name of bit-file (had .bit.bit)
 *
 *  Revision 1.10  2005/08/30 22:23:59  mickler
 *  + CHANGED: shell variable BENCHIT_KERNEL_NAME_STRING renamed to
 *    BENCHIT_KERNELNAME and made necessary changes in COMPILE.SHs and
 *    main BenchIT files (benchit.c, JBI.java, ...)
 *  - FIXED: user settings for e.g. linear measurement were not used,
 *    added the ${BENCHIT_DEFINES} to the compile commands in COMPILE.SHs
 *
 *  Revision 1.9  2005/07/29 11:52:25  wloch
 *  kernel name is no long set in bi getinfo
 *
 *  Revision 1.8  2005/07/21 19:42:13  wloch
 *  debugged parameter file parsing
 *
 *  Revision 1.7  2005/07/21 18:54:23  wloch
 *  implemented parameter file argument
 *
 *  Revision 1.6  2005/07/21 14:22:57  wloch
 *  implemented parameter parsing
 *
 *  Revision 1.5  2005/07/21 13:32:04  wloch
 *  implemented hashtable dump into bit file
 *
 *  Revision 1.4  2005/07/20 12:42:21  wloch
 *  kernellanguage and libraries are now taken from kernel name
 *
 *  Revision 1.3  2005/07/19 13:28:54  wloch
 *  changed NUMPROZ to NUMPROC
 *
 *  Revision 1.2  2005/07/19 13:23:20  wloch
 *  synchronized bit file output
 *
 *  Revision 1.1.1.1  2005/07/18 13:03:19  wloch
 *  the final restructured benchit source tree
 *
 *  Revision 1.7  2005/06/21 12:28:30  wloch
 *  added support for BENCHIT OUTPUT DIR env var
 *
 *  Revision 1.6  2005/04/12 14:37:06  wloch
 *  axis min and max is calced correctly now and treatment of not measured results works now too
 *
 *  Revision 1.5  2005/04/12 11:00:14  wloch
 *  fixed some major mistreatment of not measured resultvalues
 *
 *  Revision 1.4  2005/02/11 12:49:34  wloch
 *  removed old commented out code
 *
 *  Revision 1.3  2005/02/07 13:41:53  rschoene
 *  not yaxisnameN, but yNaxisname!!!
 *
 *  Revision 1.2  2005/01/06 12:58:28  wloch
 *  removed wrong imports
 *
 *  Revision 1.1.1.1  2004/12/14 21:22:55  william
 *  Release 3.0 - created new cvs-tree src2
 *
 *  Revision 1.1  2004/12/09 13:12:37  wloch
 *  java benchit
 *
 *  Revision 2.4  2004/08/17 13:26:14  rschoene
 *  some debug-work
 *
 *  Revision 2.3  2004/08/17 12:53:43  rschoene
 *  modified for version 3
 *
 *  Revision 2.2  2004/01/29 09:52:40  wloch
 *  die neue alte JBI
 *
 *  Revision 2.1  2004/01/28 17:40:03  wloch
 *  nutzbarer gemacht
 *
 */

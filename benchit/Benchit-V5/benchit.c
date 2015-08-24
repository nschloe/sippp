/*******************************************************************************
 * Main program of the BenchIT-project
 *
 *B e n c h I T - Performance Measurement for Scientific Applications
 */
/*!@file benchit.c
 * Main program of the BenchIT-project
 *
 * Author: Guido Juckeland (juckeland@zhr.tu-dresden.de)
 * Last change by: $Author: william $
 * $Revision: 1.77 $
 * $Date: 2007/07/11 08:03:03 $
 */
/******************************************************************************/

#include <stdio.h>

/* This is for `size_t'. */
#include <stddef.h>

/* This is for different calculations. E.g for logarithmic axis */
#include <math.h>

/* This is for a lot of String work concat and so on... */
#include <string.h>

/* used for typeconversion e.g. atoi string to integer */
#include <stdlib.h>

/* main posix header */
#include <unistd.h>

/* This is for catching SIG_INT and SIG_TERM*/
#include <signal.h>

/* if this is compiled for a MPI-kernel,
 * somewhere you got to set -DUSE_MPI as compiler flag
 * to include mpi-header
 */
#ifdef USE_MPI
 #include <mpi.h>
#endif

/* if this is compiled for a OpenMP-kernel,
 * somewhere you got to set -DUSE_OMP as compiler flag
 * to include OpenMP-header
 */
#ifdef USE_OMP
 #include <omp.h>
#endif

/* if PAPI should be used,
 * somewhere you got to set -DUSE_PAPI as compiler flag
 */
#ifdef USE_PAPI
 #include <papi.h>
#endif

/* used for timers */
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>

/* used for BenchIT */
#include "interface.h"

/* used for BenchIT version number (year) */
#ifndef BENCHIT_MAJOR
#define BENCHIT_MAJOR 2005 /**< Major version number used by fileinfo.c */
#endif

/* used for BenchIT version number (month) */
#ifndef BENCHIT_MINOR
#define BENCHIT_MINOR 06 /**< Minor version number used by fileinfo.c */
#endif

/* used for BenchIT version number (day) */
#ifndef BENCHIT_SUBMINOR
#define BENCHIT_SUBMINOR 16 /**< Subminor version number used by fileinfo.c */
#endif

/* Every kernel on every problemsize will be run (Accuracy+1) times */
#undef DEFAULT_ACCURACY
#define DEFAULT_ACCURACY 2 /**< If the LOCALDEF does not define an accuracy number - 
                2 will be used which will result in 2 repetitions 
                of each measurementstep */

/* the time-limit in seconds. when more time has passed, the measurement is interrupted */
#undef DEFAULT_TIMELIMIT
#define DEFAULT_TIMELIMIT 600  /**< Number of seconds after which a measurment is stoped 
                  (should be set in PARAMETERS) */

#define MAX_ADD_INFO_STR 4096

/*!@brief Holds information about an axis for assembling gnuplot options.
 *
 * See get_axis_properties().
 */
typedef struct axisdata
{
  /*@{*/
  /*!@brief Minimum and maximum values from the measurement. */
  double min, max;
  /*@}*/
  /*@{*/
  /*!@brief Minimum and maximum values - gnuplot option. */
  double plotmin, plotmax;
  /*@}*/
  /*!@brief Base=0 for linear axis, 2 and 10 for logarithmic axis. */
  double base;
  /*!@brief Number of ticks on the axis - gnuplot option. */
  int ticks;
  /*!@brief The incrementation value - gnuplot option. */
  double incr;
  /*!@brief Name of the axis - 'x' or 'y'. */
  char name;
} axisdata;


static const char *BIN_pref[] = { "", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei" }; /**< list of Prefixes*/
#define BIN_PREF_MAX 6 /**< number of Prefixes*/
static const char SI_prefixes[] = { 'y', 'z', 'a', 'f', 'p', 'n', 'u', 'm', ' ', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y' }; /**< list of Prefixes defined by SI */
static const char *SI_pref = &(SI_prefixes[8]); /**< pointer to the SI_prefixes-array */
#define SI_PREF_MIN (-8) /**< min number of SI-Prefixes*/
#define SI_PREF_MAX 8 /**< min number of SI-Prefixes*/

/* these values are used for getting max and mins. They will be setted when starting*/
double BI_INFINITY = 0.0;  /**< default values to be used if values are to big, set when starting main()*/
double BI_NEG_INFINITY = 0.0; /**< default values to be used if values are to small, set when starting main()*/

/*name of the file where progress information is stored to*/
static char* progf = NULL;
/*file descriptor to the file where progress information is stored to */
static FILE* prog_file = NULL;

/*
* boolean for standalone-applictation
*/
int bi_standalone=0; /**< is this a standalone binary to be used without benchit-environment*/

/* For more detailed output of what's going on. */
int verbose = 0; /**< be more communicative */

static double bi_gettimeofday(void);
static double bi_gettimeofday_improved(void);
double (*bi_gettime)() = bi_gettimeofday_improved;

double dTimerGranularity;
double dTimerOverhead;
double d_bi_start_sec; /**< start time */
static void selectTimer(void);

static void freeall(double* results); /**< give back all the memory you took */
static void safe_exit(int code);
static void get_axis_properties( axisdata *ad );
static void write_gnuplot_axisproperties( FILE *f, const axisdata *ad );

static void bi_fprintf(FILE *f,char *s); /**< own routine for writing the resultfile */
static int get_new_problems(int *todo, int *done, int max);
static void checkCommandLine( int argc, char **argv );
static void printHelpAndExit(void);
static int isOption( char** argv, int *pos, int argc, char sOpt, char *lOpt,
              int hasValue, char **value );
static void createDirStructureOrExit( const char *dirstr );
static void getKernelNameInfo( char **, char **, char ***, int *);
static float fracpart(char *r);
static void print_C_Compiler_information_to_file(FILE * bi_out, char * buf);

#include "bienvhash.c"

/*!@brief Separate the components of the kernelname.
 *
 * @param[out] kernelstring The complete name of the kernel.
 * @param[out] language Programming language of the kernel.
 * @param[out] libraries The software libraries used by the kernel.
 * @param[out] numLibraries The count of used libraries.
 */
static void getKernelNameInfo( char **kernelstring,
      char **language, char ***libraries, int *numLibraries )
{
  /*
   get the kernel name (numerical.matmul.C.0.0.double)
  */
  char *kernelname = bi_strdup( bi_getenv( "BENCHIT_KERNELNAME", 1 ) );
  /*variables:
    id:which part of the kernelname
    ps,pe:used for start and end of parts of the kernelname
    klen:kernelname-length
    pnum:number of parallel libraries
    onum:number of other libraries
    i:at the end of this procedure used for loops, which lib is written
  */ 
  int id = 0, ps = -1, pe = -1, klen = -1, pnum = 0, onum = 0, i = 0, rank = 0;
  /*
  buffer for library names
  */
  char lbuf[100000];
  /*
  parallel and other libs (limited to 10), do you really want to use more?
  */
  char *plibs[10], *olibs[10];
#ifdef USE_MPI
  /* get own rank */
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
#endif
  /* fills lbuf with 0s (NOT '0's) */
  memset( lbuf, 0, 100000 );
  /* get the length of e.g. numerical.matmul.C.0.0.double */
  klen = strlen( kernelname );
  /* set the first return value for the kernelname as copy from kernelname */
  *kernelstring = bi_strdup( kernelname );
  /* print it to cmd-line */
    if (rank == 0) printf("\nkernelname=%s, kernelstring=%s", kernelname, *kernelstring);
  /* walk through the categories */
  while ( ( pe = indexOf( kernelname, '.', ps+1 ) ) >= 0 )
  {
    id++;
    /* get kernel source language */
    if ( id == 3 )
    {
      substring( kernelname, lbuf, ps+1, pe );
      *language = bi_strdup( lbuf );
    }
    /* get libs for parallel programming */
    else if ( id == 4 )
    {
      if ( ps + 1 < klen && kernelname[ps+1] != '0' )
      {
        int nextDot = indexOf( kernelname, '.', ps + 1 );
        int lbufpos = 0;
        if ( nextDot > ps + 1 && nextDot + 1 < klen )
        {
          /* nextDot is index of category end in kernelname */
          for ( i = ps + 1; i < nextDot; i++ )
          {
            if ( kernelname[i] == '-' && kernelname[i+1] == '-' )
            {
              /* found separator */
              /* end the last found item of parallel libs */
              lbuf[lbufpos] = '\0';
              /* reset the beginning of the buffer */
              /* (the next item starts also at 0 and will overwrite the other one) */
              lbufpos = 0;
              /* add lib to list */
              plibs[pnum++] = bi_strdup( lbuf );
            }
            else {
              /* add next character to the next parallel lib-name */
              lbuf[lbufpos++] = kernelname[i];
            }
          }
          /* end the last existing item of parallel libs */
          lbuf[lbufpos] = '\0';
          /* add lib to list */
          plibs[pnum++] = bi_strdup( lbuf );
        }
      }
    }
    /* get other libs */
    else if ( id == 5 ) {
      if ( ps + 1 < klen && kernelname[ps+1] != '0' )
      {
        int nextDot = indexOf( kernelname, '.', ps + 1 );
        int lbufpos = 0;
        if ( nextDot > ps + 1 && nextDot + 1 < klen )
        {
          /* nextDot is index of category end in kernelname */
          for ( i = ps + 1; i < nextDot; i++ )
          {
            if ( kernelname[i] == '-' && kernelname[i+1] == '-' )
            {
              /* found separator */
              /* end last item of other libs */
              lbuf[lbufpos] = '\0';
              /* reset the beginning of the buffer */
              lbufpos = 0;
              /* add lib to list */
              olibs[onum++] = bi_strdup( lbuf );
            }
            else
            {
              /* add next character to the next parallel lib-name */
              lbuf[lbufpos++] = kernelname[i];
            }
          }
          /* end the last existing item of parallel libs */
          lbuf[lbufpos] = '\0';
          /* add lib to list */
          olibs[onum++] = bi_strdup( lbuf );
        }
      }
    }
    /* new start '.' is old end '.' */
    ps = pe;
  } /* end of walk through categories */
  free( kernelname );
  /* total libraries are both parallel and others */
  *numLibraries = pnum + onum;
  /* if we dont have any libraries, we still have one: NONE ;) */
  if (*numLibraries == 0) *numLibraries = 1;
  /* getting memory for libraries */
  *libraries = ( (char **)malloc( *numLibraries * sizeof( char * ) ) );
  if ( *libraries == 0 )
  {
    printf(" [FAILED] (no more memory for libs)\n" );
    safe_exit( 1 );
  }
  /* add the libs */
  id = 0;
  /* first the parallel libs */
  for ( i = 0; i < pnum; i++ )
  {
    (*libraries)[id++] = plibs[i];
  }
  /* then the other libs */
  for ( i = 0; i < onum; i++ )
  {
    (*libraries)[id++] = olibs[i];
  }
  /* sort libs. Nah. dont do it! */
  for ( id = 0; id < *numLibraries; id++ )
  {
  }
}

/*!@brief Print list and explanation of command line args on stdout
 *        and exit the program.
 */
static void printHelpAndExit(void)
{
  printf( "Usage: <executable> [option]...\n\n" );
  printf( "Where option can be:\n" );
  printf( " -h, --help\t\t\t" );
  printf( "show this help screen\n" );
  printf( " -d, --dumpTable\t\t" );
  printf( "print the environment variables stored in the internal"
          "\n\t\t\t\tHashTable to stdout\n" );
  printf( " -o, --output-dir=DIR\t\t" );
  printf( "write result files into DIR\n" );
  printf( " -S, --standalone\t\t\t" );
  printf( "don't read _input_* (you should also set -o!)\n" );
  printf( " -p, --parameter-file=PAR_FILE\t" );
  printf( "read parameters from PAR_FILE at runtime\n" );
  printf( " -q, --quiet\t\t\t" );
  printf( "suppress all messages to stdout and stderr\n" );
  printf( " -v, --verbose\t\t\t" );
  printf( "print more messages about what the program is doing\n" );
  printf( " -V, --version\t\t\t" );
  printf( "print version information\n" );

  fflush( stdout );
  safe_exit( 0 );
}

/*!@brief Extract values for recognized options.
 *
 * Checks if at position pos in argv is an option in whether
 * short or long version. If hasValue is greater than 0 the
 * pointer value will be set to the start of the value string
 * and the varibale pos will be increased.
 * If argv[i] is a short option the value is expected in argv[i+1].
 * If argv[i] is a long option, the value is that part of argv[i]
 * that follows the first occuring = sign.
 * @param[in] argv the argv array.
 * @param[in] pos the postion in argv to look at.
 * @param[in] argc the number of elements in argv.
 * @param[in] sOpt the short version of the option.
 * @param[in] lOpt the long version of the option.
 * @param[in] hasValue 0 if option without value, 1 or greater if with value.
 * @param[in,out] value pointer to the value char array.
 * @return 1 if successful match and retrieval of value, 0 else.
 */
static int isOption( char** argv, int *pos, int argc, char sOpt, char *lOpt,
              int hasValue, char **value )
{
  /*
    retval is the return value for this function
    len is the length of the poss entry in argv.
  */
  int retval = 0, len = -1;
  /* reset value */
  *value = 0;
  /* if there is no argument number pos return 0 */
  if ( argv[*pos] == 0 ) return retval;
  /* else get the length of the argument */
  len = lengthc( argv[*pos] ) + 1;
  /* if the pos' arguments first char is not a '-' return 0 */
  if ( argv[*pos][0] != '-' ) return retval;
  /* now try to match option
   * ! short Options MUST have length 2 ('-' plus a single character)
   */
  if ( len == 2 )
  {
    /* short option */
    /* if there was some strange stuff used as short Option return 0 */
    if ( sOpt == 0 ) return retval;
    /* if there is no Value needed for this option, but a value is passed return 0 */
    if ( hasValue > 0 && *pos + 1 >= argc ) return retval;
    /* if it is found */
    if ( argv[*pos][1] == sOpt )
    {
      /* short option hit */
      /* if it needs a value, set it */
      if ( hasValue == 1 ) *value = argv[++*pos];
      /* but always return 1 */
      retval = 1;
    }
    /* if it is not found return 0 */
    else return retval;
  }
  /*
  * ! long options MUST have length >2 (- plus at least 2 chars)
  */
  else if ( len > 2 && lOpt != 0 )
  {
    /* long option */
    /* STR_LEN is passed from a header (string.h?) */
    char sub[STR_LEN];
    /* position */
    int eqPos = -1;
    /* fill the string sub with 0s (NOT with '0's) */
    memset( sub, 0, STR_LEN );
    /* if there is no lOpt passed */
    if ( lOpt == 0 ) return retval;
    /* if the argument doesn't start with a dash '-' */
    if ( argv[*pos][1] != '-' ) return retval;
    /* if it needs a value, they must be separated by a = : -option=value */
    /* always point separation between arguments: */
    if ( hasValue == 1 )
    {
      eqPos = indexOf( argv[*pos], '=', 1 );
    }
    else
    {
      eqPos = len + 1;
    }
    /* if it is too short (only a single char) return 0 */
    if ( eqPos < 3 ) return retval;
    /* extract option name */
    /* if the option name cannot be found in the option
     * (sub=argv[*pos].substring(2,eqPos))
     * subString sub is a substring of argv[*pos] beginning with the 2nd char to the eqPos' char
     * if the creation of this substring fails retun 0 */
    if ( substring( argv[*pos], sub, 2, eqPos ) != 1 ) return retval;
    /* if the subString is to short return false */
    if ( sub == 0 ) return retval;
    /* compare the strings 0 means no differences */
    if ( comparec( sub, lOpt ) == 0 )
    {
      /* long option hit */
      /* if it needs a value (-option=value), set it */
      if ( hasValue == 1 ) *value = &argv[*pos][eqPos+1];
      /* but always return 0 */
      retval = 1;
    }
  }
  /* if nothing is found return 0; */
  else return retval;
  return retval;
}

/*!@brief Parse commandline arguments.
 *
 * Checks the arguments for recognizable input. Does not use
 * getopt for compatibility reasons. See documentation of
 * printUsage() for detailed info about recognized options.
 * @param argc argc from main()
 * @param argv argv from main()
 */
static void checkCommandLine( int argc, char **argv )
{
  /* for incrementing over arguments */
  int i;
  /* increment over arguments:
   * (all possible arguments should be checked here)
   * check: is this an available option? */
  for ( i = 1; i < argc; i++ )
  {
    /* value of the option (if it needs one) */
    char *value = 0;
    /* shall the help be printed? */
    if ( isOption( argv, &i, argc, 'h', "help", 0, &value ) == 1 )
    {
      printHelpAndExit(); continue;
    }
    /* shall the environment be dumped? */
    if ( isOption( argv, &i, argc, 'd', "dumpTable", 0, &value ) == 1 )
    {
      bi_dumpTable();
      fflush( stdout ); safe_exit( 0 ); continue;
    }
    /* shall the output-dir be setted? */
    if ( isOption( argv, &i, argc, 'o', "output-dir", 1, &value ) == 1 )
    {
      bi_put( bi_strdup( "BENCHIT_RUN_OUTPUT_DIR" ), value );
      continue;
    }
    /* shall another parameter file be used? */
    if ( isOption( argv, &i, argc, 'p', "parameter-file", 1, &value ) == 1 )
    {
      bi_put( bi_strdup( "BENCHIT_PARAMETER_FILE" ), value );
      bi_readParameterFile( value );
      continue;
    }
    /* shall there be no printing? */
    if ( isOption( argv, &i, argc, 'q', "quiet", 0, &value ) == 1 )
    {
      if( freopen( "/dev/null", "w", stdout ) == NULL )
      {
        printf( "BenchIT: Error: could not remap stdout to /dev/null.\n" );
        fflush( stdout );
        safe_exit( 1 );
      }
      if( freopen( "/dev/null", "w", stderr ) == NULL )
      {
        printf( "BenchIT: Error: could not remap stderr to /dev/null.\n" );
        fflush( stdout );
        safe_exit( 1 );
      }
      continue;
    }
    /* shall we use verbose mode? */
    if ( isOption( argv, &i, argc, 'v', "verbose", 0, &value ) == 1 )
    {
      verbose = 1; continue;
    }
    /* or print the version? */
    if ( isOption( argv, &i, argc, 'V', "version", 0, &value ) == 1 )
    {
      printf( "BenchIT version %d.%d.%d\n",
              BENCHIT_MAJOR, BENCHIT_MINOR, BENCHIT_SUBMINOR );
      fflush( stdout ); safe_exit( 0 );
    }
    /* or shall we run this as standalone? */
    if ( isOption( argv, &i, argc, 'S', "standalone", 0, &value ) == 1 )
    {
      bi_standalone=1;
      continue;
    }
    /* if this point of the loop is reached, the argument
       is not a recognized option */
    printf( "BenchIT: Unknown argument: %s\n", argv[i] );
    safe_exit( 1 );
  }
}

/*!@brief Safely exit BenchIT.
 *
 * Cleans up before exit is called.
 * COMMENTS: set -DVAMPIR_TRACE while compiling to clean vampir-trace to!
 * @param code The exitcode to use with exit().
 */
static void safe_exit( int code )
{
#ifdef USE_MPI
  if (code == 0)
    MPI_Finalize();
  else
    MPI_Abort(MPI_COMM_WORLD, code);
#endif
#ifdef VAMPIR_TRACE
  (void) _vptleave(300);
  (void) _vptflush();
#endif

  if (prog_file!=NULL)
  {
      fclose(prog_file);
      unlink(progf);
  }

  exit( code );
}

/*!@brief Generate a new todolist with problemsizes for computation by the
 *        kernel.
 *
 * PRE: in the first call todo and done are arrays with all values=0
 * max = maximumproblemsize = length of the arrays
 * todo[0] and done[0] remain unchanged by the calling party
 * done[i] indicates, that the problemsize i has already been calculated
 * POST:
 * Short: todo is a new array of problemsizes to be calculated
 * the array is terminated with a 0
 * Long:
 *    after calling this function todo[i] (i!=0) will contain the problemsize(s),
 *    which shall be computed in the next step(s). It is NOT said how many todo-problemsizes are returned!
 *    todos after the last valid todo[i] will contain a problemsize of 0
 * functions returns true if new problems could be generated, false if not
 * COMMENTS: todo[0] is used to store the number of calls of the function get_new_problems
 * done[0] iw used to store whether we are done or not, done[0]=1 means, that get_new_problems
 * will return true with the next call
 * @return 1 if new todolist could be generated\n
 *         0 if there are no problemsizes left.
 */
static int get_new_problems(int *todo, int *done, int max)
{
  /* todo[0] contains the number of calls of this function. */
  /* heres another call, so increment it */
  /* though we are filling first the middle problemsize, then the ones at quarters */
  /* and so on, we check for the half/quarter/eighth, which is computed by max/(2^num_ofCall) */
  int stepsize = (int)(max / (int)pow(2,(++todo[0])));
  /* those are used for for loops, explained later */
  int i = 0, k = 1, inc = 0;
  /* when in the last call of this function done[0] was NOT set, there is still sth. to do */
  int ret = !done[0];
  /* but if it was set in the last call, we're done */
  if (ret==0)
    return ret;
  IDL(1,printf("Entering get_new_problems for %d. time\n",todo[0]));
  /* if the difference is small enough (dont compute for 1/(2^1024)-parts) */
  /* go to linear stepping */
  if( stepsize < (0.05*max) )
    stepsize = 1;
    /* if linear measurement is used, do linear stepping too :P */
#ifdef LINEAR_MEASUREMENT
  stepsize = 1;
#endif
  /* if we take every following problemsize... */
  if( stepsize == 1 )
    /* ... we should also set inc(rement) to 1, to reach all problemsizes */
    inc = 1;
  /* if we are still in the pattern which computes first the half problemsize, then the quarter and three-quarter, ...   */
  else
    /* if e.g. it is the 2nd call of the function, the middle problemsize has been solved. */
    /* BUT now we have to solve those at the quarter and three-quarter */
    /* so stepsize is (1/4)*problemsizemax, but inc is (1/2)*problemsizemax */
    /* to reach (1/4)*problemsizemax AND (3/4)*problemsizemax :) */
    inc = 2 * stepsize;
  /* now compute the problemsizes, which shall be generated */
  /* the first will be k[1], the 2nd k[2] and so on */
  for(i=stepsize;i<=max;i+=inc)
  {
    if (done[i]!=1) todo[k++]=i;
  }
  /* the first item after all todo-problemsizes is set to 0 */
  /* maybe in the last step we had to compute more then in this step */
  todo[k]=0;
  /* if stepsize is 1, means, that all remaining problemsizes were written */
  /* into the todo field */
  if( stepsize == 1 )
    done[0] = 1;
  /* if we have a larger stepsize: don't do the measurement for largest problemsize */
  /* this time */
  else if( todo[k-1] == max )
    todo[k-1] = 0; /*remove max from todo unless stepsize=1*/
  IDL(2,printf("todolist="));
  for( i=1; i<=max; i++ )
    IDL(2,printf(" %d",todo[i]));
  IDL(2,printf("\n"));
  IDL(1,printf("Leaving get_new_problems.\n"));
  return ret;
}


/**
 * variables used by main(), analyse_results() and write_results() 
 * defined static to not be visible in other source-files (avoid multiple declarations)
 */
 /*
  *
  */
  static void *mcb;
 /*
  * variables:
  * rank: used for MPI, will be the rank of the MPI-process or 0 if no MPI is used
  * offset: how many functions are measured by bi_entry
  * timelimit: timelimit for running the benchmark, if exceeding this limit, it will be stopped
  * accuracy: how often every kernel is measured
  */
  static int rank,offset=0,v=1,w=1,flag=0,i=0,j=0,n=0,timelimit,accuracy;
 /*
  * info about the kernel, will be filled by kernel
  */
  static bi_info theinfo;
 /*
  * will contain all results of all bi_entry_call for one problemsize
  */
  static double *allresults=NULL;
  /*
  * will contain data for all axis (mins, maxs, ...)
  */
  static axisdata xdata, *ydata = NULL, ydata_global;
 /* variables:
  * p: used as temporary variable for reading environment variables
  * str: used for building the output-file name
  * filename: used for name of output file
  * filename2: used for name of output file
  * dirstr: used for directory name
  * used for benchits root directory
  */
  static char *p=NULL, *str=NULL, *filename=NULL, *filename2=NULL, *dirstr=NULL, *rootdir=NULL;
 /*
  * will contain the current time for filename
  */
  static time_t currtime;
  static struct tm *currtm;
 /*
  * a buffer used for file output
  */
  static char buf[100000];
 /*
  * will contain the parts of the kernel name and the kernelstring
  */
  static char *language = NULL, **libraries = NULL, *kernelstring = NULL;
 /*
  * will contain the number of used libraries
  */
  static int numLibraries = 0;
 /*
  * used for file work (input and output stream)
  */
  static FILE *bi_in, *bi_out;



/*!****************************************************************************
 * Analyzing results (Getting Min, Max)
 */
static void analyse_results(void)
{
    printf("BenchIT: Analyzing results..."); fflush(stdout);

    /* initialize mins and maxs */
    xdata.min=BI_INFINITY;
    xdata.max=BI_NEG_INFINITY;
    for(j=0;j<theinfo.numfunctions;j++)
    {
      ydata[j].min=BI_INFINITY;
      ydata[j].max=BI_NEG_INFINITY;
    }

    /* find mins and maxs for x-axis and y-axis within the results */
    for(i=0;i<theinfo.num_measurements;i++)
    {
      if (allresults[i*offset]<=0) continue; /* =0: not measured (timeout/abort) <0 : invalid*/

      /* set min for xaxis */
      if(( allresults[i*offset] < xdata.min )&&(allresults[i*offset]>BI_NEG_INFINITY))
        xdata.min=allresults[i*offset];
      /* set max for xaxis */
      if(( allresults[i*offset] > xdata.max )&&( allresults[i*offset]<BI_INFINITY))
        xdata.max=allresults[i*offset];
      for( j=0;j<theinfo.numfunctions;j++ )
      {
        if(( allresults[i*offset+j+1] > INVALID_MEASUREMENT )||( allresults[i*offset+j+1] < INVALID_MEASUREMENT ))
        {
          /* set min for yaxis[j] */
          if(( allresults[i*offset+j+1] < ydata[j].min )&&(allresults[i*offset+j+1]>BI_NEG_INFINITY))
            ydata[j].min=allresults[i*offset+j+1];
          /* set max for yaxis[j] */
          if(( allresults[i*offset+j+1] > ydata[j].max )&&( allresults[i*offset]<BI_INFINITY))
            ydata[j].max=allresults[i*offset+j+1];
        }
      }
    }

    /* Get global minimum and maximum for y-axis */
    ydata_global.min = BI_INFINITY;
    ydata_global.max = BI_NEG_INFINITY;
    for( i=0; i<theinfo.numfunctions; ++i )
    {
      if( ydata[i].min < ydata_global.min )
        ydata_global.min = ydata[i].min;
      if( ydata[i].max > ydata_global.max )
        ydata_global.max = ydata[i].max;
    }

    /* adjust min and max of x-axis if we only have one measurement */
    if (xdata.min==xdata.max) {xdata.max++;xdata.min--;}
    printf(" [OK]\n");
}

/*!****************************************************************************
 * Write *.bit-file (resultfile)
 * and *.bit.gp file (gnuplot file for quickview)
 */
static void write_results(void)
{
  int empty = 1,i;
  for(i=0;i<theinfo.num_measurements;i++)
  {
    if (allresults[i*offset]>0) {empty=0;break;} /* <=0: invalid */
  }
  if (!empty)
  {
      printf("BenchIT: Writing resultfile..."); fflush(stdout);
      IDL(2,printf("\nWriting measurementinfos"));
      /* composing result file name */
      str=(char*)malloc(sizeof(char)*300);
      if (str==0)
      {
        if (rank==0)
        printf(" [FAILED]\nBenchIT: No more memory. ");
        freeall(allresults);
        safe_exit(1);
      }
      str[0]=0;
      /* start with ARCH_SHORT */
      p=bi_getenv("BENCHIT_ARCH_SHORT",0);
      if ((p==0)||(bi_standalone))
      {
        (void)strcat(str,"unknown");
      }
      else
      (void)strcat(str,p);
      (void)strcat(str,"_");
      /* then ARCH_SPEED */
      p=bi_getenv("BENCHIT_ARCH_SPEED",0);
      if ((p==0)||(bi_standalone))
      {
        (void)strcat(str,"unknown");
      }
      else
        (void)strcat(str,p);
      (void)strcat(str,"__");
  
      /* The comment: BENCHIT_FILENAME_COMMENT */
      p=bi_getenv( "BENCHIT_FILENAME_COMMENT", 0 );
      if (p==0)
        (void)strcat(str,"0");
      else
        (void)strcat(str,p);
      (void)strcat(str,"__");
      /* add date and time */
      currtime = time((time_t *) 0);
      currtm = localtime(&currtime);
      (void)strftime(buf,(size_t)sizeof(buf),"%Y_%m_%d__%H_%M_%S.bit",currtm);
      (void)strcat(str,buf);
      (void)strcat(str,"\0");
      filename=(char*)malloc(sizeof(char)*300);
      (void)strcpy(filename,str);
      /* composing output directory name */
      filename2=bi_strdup(str);
      dirstr=(char*)malloc(sizeof(char)*300);
      memset( dirstr, 0, sizeof( dirstr ) );
      if (dirstr==0)
      {
        if (rank==0)
          printf(" [FAILED]\nBenchIT: No more memory. ");
        freeall(allresults);
        safe_exit(1);
      }
      dirstr[0]=0;
      p=bi_getenv("BENCHIT_RUN_OUTPUT_DIR",1); /* exit if default not set */
      IDL(5,printf("\noutput-dir=%s",p));
      (void)strcat(dirstr,p);
      (void)strcat(dirstr,"/");
      p = bi_strdup( kernelstring );
      if ( p == 0 )
      {
        if ( rank == 0 )
          printf(" [FAILED]\nBenchIT: No kernelstring in info struct set. ");
        freeall(allresults);
        safe_exit(1);
      }
      /* replace all dots by / in p */
      for( i = 0; i <= length(p); i++ )
      if( p[i]=='.' ) p[i]='/';
      (void)strcat(dirstr,p);
      free( p );
      flag=chdir(dirstr);
      if (flag!=0)
      {
        createDirStructureOrExit( dirstr );
        if ( chdir(dirstr) != 0 )
        {
          if (rank==0)
            printf(" [FAILED]\nBenchIT: Couldn't change to output directory: %s.\n", dirstr);
          freeall(allresults);
          safe_exit(127);
        }
      }
      bi_out=fopen(str,"w");
      if (bi_out==0)
      {
        if (rank==0) printf(" [FAILED]\nBenchIT: could not create output-file \"%s\"\n",filename);
        safe_exit(127);
      }
      sprintf(buf,"# BenchIT-Resultfile\n#\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,"# feel free to fill in more architectural information\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,"#########################################################\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,"beginofmeasurementinfos\n");
      bi_fprintf(bi_out,buf);
      memset( buf, 0, 100000 );
      if (kernelstring != 0)
        sprintf(buf,"kernelstring=\"%s\" \n", kernelstring);
      else
        sprintf(buf,"kernelstring=\n");
      /*bi_fprintf(bi_out, buf);*/
      fprintf(bi_out, "%s", buf);
      (void)strftime(buf,(size_t)sizeof(buf),"%b %d %H:%M:%S %Y",currtm);
      p=(char*)malloc(sizeof(buf));
      if (p==0)
      {
        if (rank==0) printf(" [FAILED]\nBenchIT: No more memory. ");
        freeall(allresults);
        safe_exit(1);
      }
      (void)strcpy(p,buf);
      sprintf(buf,"date=\"%s\"\n",p);
      free(p);
      bi_fprintf(bi_out,buf);
      p = bi_getenv("BENCHIT_NUM_CPUS",0);
      if (p!=0)
      sprintf(buf,"numberofprocessorsused=%s\n",p);
      else
      sprintf(buf,"numberofprocessorsused=\n");
      bi_fprintf(bi_out,buf);
      p = bi_getenv("BENCHIT_RUN_MAX_MEMORY",0);
      if (p!=0)
      sprintf(buf,"memorysizeused=\"%s\"\n",p);
      else
      sprintf(buf,"memorysizeused=\n");
      bi_fprintf(bi_out,buf);
      p = bi_getenv ("BENCHIT_KERNEL_COMMENT",0);
      if (p!=0)
      sprintf(buf,"comment=\"%s\"\n", p);
      else
      sprintf(buf,"comment=\n");
      bi_fprintf(bi_out, buf);
      if( theinfo.kerneldescription )
        sprintf( buf, "kerneldescription=\"%s\"\n", theinfo.kerneldescription );
      else
        sprintf( buf, "kerneldescription=\n" );
      bi_fprintf(bi_out, buf);
      if (language != 0)
      sprintf(buf,"language=\"%s\"\n",language);
      else
      sprintf(buf,"language=\n");
      bi_fprintf(bi_out,buf);
      p = bi_getenv ("BENCHIT_COMPILER",0); /**<\brief old Variablename \deprecated */
      if (p!=0)
      sprintf(buf,"compiler=\"%s\"\n", p);
      else{
        p = bi_getenv ("LOCAL_KERNEL_COMPILER",0);
        if (p!=0) sprintf(buf,"compiler=\"%s\"\n", p);
        else sprintf(buf,"compiler=\n");
      }
      bi_fprintf(bi_out,buf);
      if (p)
      {
        if (!strcmp(p,bi_getenv("BENCHIT_CC",0))) sprintf(buf,"compilerversion=\"%s\"\n", bi_getenv("BENCHIT_CC_COMPILER_VERSION",0));
        else if (!strcmp(p,bi_getenv("BENCHIT_CXX",0))) sprintf(buf,"compilerversion=\"%s\"\n", bi_getenv("BENCHIT_CXX_COMPILER_VERSION",0));
        else if (!strcmp(p,bi_getenv("BENCHIT_F77",0))) sprintf(buf,"compilerversion=\"%s\"\n", bi_getenv("BENCHIT_F77_COMPILER_VERSION",0));
        else if (!strcmp(p,bi_getenv("BENCHIT_F90",0))) sprintf(buf,"compilerversion=\"%s\"\n", bi_getenv("BENCHIT_F90_COMPILER_VERSION",0));
        else if (!strcmp(p,bi_getenv("BENCHIT_F95",0))) sprintf(buf,"compilerversion=\"%s\"\n", bi_getenv("BENCHIT_F95_COMPILER_VERSION",0));
        else sprintf(buf,"compilerversion=\n");
      } else sprintf(buf,"compilerversion=\n");
      bi_fprintf(bi_out,buf);
      p = bi_getenv ("BENCHIT_COMPILERFLAGS",0); /**<\brief old Variablename \deprecated */
      if (p!=0)
      sprintf(buf,"compilerflags=\"%s\"\n", p);
      else{
        p = bi_getenv ("LOCAL_KERNEL_COMPILERFLAGS",0);
        if (p!=0) sprintf(buf,"compilerflags=\"%s\"\n", p);
        else sprintf(buf,"compilerflags=\n");
      }
      bi_fprintf(bi_out,buf);
  
      /* print C compiler infos */
      print_C_Compiler_information_to_file(bi_out,buf);
      /* write kernellibraries*/
      for ( i = 0; i < numLibraries; i++ )
      {
        if ( libraries[i] == 0 )
        sprintf( buf, "library%d=\"\"\n", i + 1 );
        else
        sprintf( buf, "library%d=\"%s\"\n", i + 1, libraries[i] );
        bi_fprintf( bi_out, buf );
      }
  
      /* check if kernel is parallel */
      i=0;
      i+=theinfo.kernel_execs_mpi1;
      i+=theinfo.kernel_execs_mpi2;
      i+=theinfo.kernel_execs_pvm;
      i+=theinfo.kernel_execs_omp;
      i+=theinfo.kernel_execs_pthreads;
      if (i==0)
      sprintf(buf,"kernelisparallel=0\n");
      else
      sprintf(buf,"kernelisparallel=1\n");
      bi_fprintf(bi_out,buf);
  
      /* write kernellibraries even if kernel has not declared them */
      if ((i!=0) && (numLibraries==0))
      {
        i=0;
        if ((theinfo.kernel_execs_mpi1) || (theinfo.kernel_execs_mpi2))
        {
          sprintf(buf,"library%d=\"MPI\"\n",++i);
          bi_fprintf(bi_out,buf);
        }
        if (theinfo.kernel_execs_pvm)
        {
          sprintf(buf,"library%d=\"PVM\"\n",++i);
          bi_fprintf(bi_out,buf);
        }
        if (theinfo.kernel_execs_omp)
        {
          sprintf(buf,"library%d=\"OpenMP\"\n",++i);
          bi_fprintf(bi_out,buf);
        }
        if (theinfo.kernel_execs_pthreads)
        {
          sprintf(buf,"library%d=\"PThreads\"\n",++i);
          bi_fprintf(bi_out,buf);
        }
      }

      if( theinfo.additional_information)
      {
        int loop;
        for (loop=0;loop<MAX_ADD_INFO_STR;loop++)
        {
          if(theinfo.additional_information[loop]=='\0') break;
        }
        if (loop<MAX_ADD_INFO_STR) sprintf( buf, "%s\n", theinfo.additional_information );
      }
      else
      bi_fprintf(bi_out, buf);
      sprintf(buf,"is3d=%i\n", theinfo.is3d);
      bi_fprintf(bi_out,buf);
      if (theinfo.codesequence != 0)
        sprintf(buf,"codesequence=\"%s\"\n", theinfo.codesequence);
      else
        sprintf(buf,"codesequence=\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,"xinmin=%g\n",xdata.min);
      bi_fprintf(bi_out,buf);
      sprintf(buf,"xinmax=%g\n",xdata.max);
      bi_fprintf(bi_out,buf);
      for(i=0;i<theinfo.numfunctions;i++)
      {
        sprintf(buf,"y%dinmin=%g\n",i+1,ydata[i].min);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"y%dinmax=%g\n",i+1,ydata[i].max);
        bi_fprintf(bi_out,buf);
      }
      sprintf(buf,"endofmeasurementinfos\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,"################################################\n");
      bi_fprintf(bi_out,buf);
      if (!bi_standalone)
      {
        IDL(2,printf("...OK\nWriting architectureinfos"));
        /* old code: (void)uname(&uname_res); */
        rootdir=bi_getenv("BENCHITROOT",0);
        if (rootdir==0)
        {
          printf(" [FAILED]\nBenchIT: BENCHITROOT not found in environment hash table.\n");
          fclose(bi_out);
          freeall(allresults);
          safe_exit(1);
        }
        p=bi_getenv("BENCHIT_NODENAME",1);
        sprintf(buf,"%s/LOCALDEFS/%s_input_architecture", rootdir, p);
        bi_in=fopen(buf,"r");
        if (bi_in==0)
        {
          sprintf(buf,"%s/LOCALDEFS/PROTOTYPE_input_architecture", rootdir);
          bi_in=fopen(buf,"r");
        }
        if (bi_in==0)
        {
          printf(" [FAILED]\nBenchIT: Cannot open input file \"input_architecture\"\n");
          fclose(bi_out);
          freeall(allresults);
          safe_exit(1);
        }
        sprintf(buf,"beginofarchitecture\n# Architekturangaben\n");
        bi_fprintf(bi_out,buf);
    
        if (p==0)
          sprintf(buf,"nodename=\n");
        else
          sprintf(buf,"nodename=\"%s\"\n",p);
        bi_fprintf(bi_out,buf);
    
        p=bi_getenv("BENCHIT_HOSTNAME",0);
        if (p==0)
          sprintf(buf,"hostname=\n");
        else
          sprintf(buf,"hostname=\"%s\" \n",p);
        bi_fprintf(bi_out,buf);
        while (fgets(buf,sizeof(buf)-1,bi_in) != (char *) 0)
        {
          fprintf(bi_out,buf);
        }
        fclose(bi_in);
        IDL(2,printf("...OK\nWriting displayinfos"));
      }
      else
      {
        sprintf(buf,"beginofarchitecture\n# Architekturangaben\n");
        bi_fprintf(bi_out,buf);
        p=bi_getenv("BENCHIT_NODENAME",1);
        if (p==0)
          sprintf(buf,"nodename=\n");
        else
          sprintf(buf,"nodename=\"%s\"\n",p);
        bi_fprintf(bi_out,buf);
        p=bi_getenv("BENCHIT_HOSTNAME",0);
        if (p==0)
          sprintf(buf,"hostname=\n");
        else
          sprintf(buf,"hostname=\"%s\" \n",p);
        bi_fprintf(bi_out,buf);
        IDL(2,printf("Running standalone. Don't write architecture_info...OK\nWriting displayinfos"));
      }
      bi_dumpTableToFile( &bi_out );
      IDL(2,printf("Writing displayinfos"));
      sprintf(buf,"beginofdisplay\n");
      bi_fprintf(bi_out,buf);
      IDL(2,printf(" x-axis "));
      xdata.base = theinfo.base_xaxis;
      get_axis_properties( &xdata );
      sprintf(buf,"\nxoutmin=%g\n", xdata.plotmin );
      bi_fprintf(bi_out,buf);
      sprintf(buf,"xoutmax=%g\n", xdata.plotmax );
      bi_fprintf(bi_out,buf);
      sprintf(buf,"xaxisticks=%d\n", xdata.ticks );
      bi_fprintf(bi_out,buf);
      sprintf(buf,"xaxislogbase=%g\n", xdata.base );
      bi_fprintf(bi_out,buf);
      if (theinfo.xaxistext==0)
        sprintf(buf,"xaxistext=\n");
      else
        sprintf(buf,"xaxistext=\"%s\"\n",theinfo.xaxistext);
      bi_fprintf(bi_out,buf);
      /* now the y-axises */
      IDL(2,printf(" y-axis "));
      for (i=0;i<theinfo.numfunctions;i++)
      {
        ydata[i].base = theinfo.base_yaxis[i];
        if( i == 0 )
          ydata_global.base = theinfo.base_yaxis[i];
        get_axis_properties( &(ydata[i]) );
        IDL(2,printf(" . " ));
        sprintf(buf,"\ny%doutmin=%g\n",i+1, ydata[i].plotmin);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"y%doutmax=%g\n",i+1, ydata[i].plotmax);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"y%daxisticks=%d\n",i+1, ydata[i].ticks);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"y%daxislogbase=%g\n",i+1, ydata[i].base);
        bi_fprintf(bi_out,buf);
        /* write axistexts */
        if (theinfo.yaxistexts[i]==0)
          sprintf(buf,"y%daxistext=\n",i+1);
        else
          sprintf(buf,"y%daxistext=\"%s\"\n",i+1,theinfo.yaxistexts[i]);
        bi_fprintf(bi_out,buf);
      }
  
      sprintf(buf,"\nnumabscissae=%d\n",v);
      bi_fprintf(bi_out,buf);
      sprintf(buf,"numfunctions=%d\n\n",theinfo.numfunctions);
      bi_fprintf(bi_out,buf);
      sprintf(buf,"## Fuer Architektur- oder Messmerkmal : abc\n#\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# displayabc=1                                           # Steuervariable\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# #Textfeld-Eigenschaften des date-Strings               # Kommentar\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# tabc=\"ABC-Merkmal: 3 Tm\"                               # Text: Wert\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# xabc=                                                  # x-Position\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# yabc=                                                  # y-Position\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# fonttypabc=                                            # Schriftart\n");
      bi_fprintf(bi_out,buf);
      sprintf(buf,
              "# fontsizeabc=                                           # Schriftgroesse\n");
      bi_fprintf(bi_out,buf);
      for( i=0; i<theinfo.numfunctions; i++ )
      {
        if( (i == 0) && (theinfo.legendtexts == 0) )
        {
          (void) fprintf (stderr, "BenchIT: info->legendtexts==NULL\n");
          (void) fflush (stderr);
          (void) safe_exit (127);
        }
        if (theinfo.legendtexts[i]==0)
        sprintf(buf,"tlegendfunction%d=\n",i+1);
        else
        sprintf(buf,"tlegendfunction%d=\"%s\"\n",i+1,theinfo.legendtexts[i]);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"xlegendfunction%d=\n",i+1);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"ylegendfunction%d=\n",i+1);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"fonttypelegendfunction%d=\n",i+1);
        bi_fprintf(bi_out,buf);
        sprintf(buf,"fontsizelegendfunction%d=\n\n",i+1);
        bi_fprintf(bi_out,buf);
      }
      if (!bi_standalone)
      {
        p=bi_getenv("BENCHIT_NODENAME",1);
        sprintf(buf,"%s/LOCALDEFS/%s_input_display", rootdir, p);
        bi_in=fopen(buf,"r");
        if( bi_in == 0 )
        {
          sprintf(buf,"%s/LOCALDEFS/PROTOTYPE_input_display", rootdir);
          bi_in=fopen(buf,"r");
        }
        if( bi_in == 0 )
        {
          printf(" [FAILED]\nBenchIT: Cannot open input file \"input_display\"\n");
          fclose(bi_out);
          freeall(allresults);
          safe_exit(1);
        }
        buf[0]=0;
        while( fgets(buf,sizeof(buf)-1,bi_in) != (char *) 0)
        {
          fprintf(bi_out,buf);
        }
        fclose(bi_in);
      } else
      {
        IDL(2,printf(" (run as standalone) "));
      }
      IDL(2,printf("...OK\nWriting Data...\n"));
      sprintf(buf,"beginofdata\n");
      bi_fprintf(bi_out,buf);
      for(i=0;i<theinfo.num_measurements;i++)
      { 
        if (allresults[i*offset]<=0) 
        { 
          if (allresults[i*offset]<0) printf("\n Warning: Problemsize %f < 0.0 - ignored",allresults[i*offset]);
          continue;
        }
        for (j=0;j<offset;j++)
        {
          if ((allresults[i*offset+j] > INVALID_MEASUREMENT)||(allresults[i*offset+j] < INVALID_MEASUREMENT))
          fprintf(bi_out,"%g\t",allresults[i*offset+j]);
          else
          fprintf(bi_out,"-\t");
        }
        fprintf(bi_out,"\n");
      }
      sprintf(buf,"endofdata\n");
      bi_fprintf(bi_out,buf);
      IDL(2,printf("...OK\n"));
      fclose(bi_out);
      printf(" [OK]\nBenchIT: Wrote output to \"%s\" in directory\n",filename2);
      printf("         \"%s\"\n", dirstr); fflush(stdout);
      free(dirstr);

      printf("BenchIT: Writing quickview file..."); fflush(stdout);
      strcat(filename,".gp");
      bi_out=fopen(filename,"w");
      fprintf(bi_out,"#gnuplotfile\n");
      fprintf(bi_out,"set title \"%s\"\n", kernelstring != 0 ? kernelstring : "");
      fprintf(bi_out,"set xlabel \"%s\"\n", theinfo.xaxistext != 0 ? theinfo.xaxistext : "");
      if( (xdata.ticks != 0) )
      {
        write_gnuplot_axisproperties( bi_out, &xdata );
      }
      get_axis_properties( &ydata_global );
      if( (ydata_global.ticks != 0) )
      {
        write_gnuplot_axisproperties( bi_out, &ydata_global );
      }
      fprintf(bi_out,"set ylabel \"%s\"\n",
              (theinfo.yaxistexts != 0) && (theinfo.yaxistexts[0] != 0) ? theinfo.yaxistexts[0] : "");
      if (bi_getenv("BENCHIT_LINES",0))
        fprintf(bi_out,"set data style linespoints\n");
      else
        fprintf(bi_out,"set data style points\n");
      fprintf(bi_out,"set term postscript eps color solid\n");
      strcat(filename,".eps");
      fprintf(bi_out,"set output \"%s\"\n",filename);
      if (theinfo.gnuplot_options != NULL)
        fprintf(bi_out,"%s\n", theinfo.gnuplot_options);
      for (i=0;i<theinfo.numfunctions;i++)
      {
        if( i != 0 )
          fprintf(bi_out,",");
        else
          fprintf(bi_out,"plot");
        fprintf(bi_out," \"%s\" using 1:%d title '%s'",filename2,i+2,
                theinfo.legendtexts[i] != 0 ? theinfo.legendtexts[i] : "");
      }
      fclose(bi_out);
      printf(" [OK]\n"); fflush(stdout);
    }
  else printf("\nerror: No output data found, not writing result files\n");
}

/**
 * Signal handler for SIGINT
 */
static void sigint_handler (int signum)
{
 char c='\0',*p=NULL;
 int interactive=0,exitcode=0;

 /* ignore further signals while handling */
 signal(signum, SIG_IGN);

 if (rank==0)
 {
   p=bi_getenv("BENCHIT_INTERACTIVE",0);
   interactive=atoi(p);
   if (interactive)
   {
     printf("\nBenchIT: Received SIGINT (Ctrl-C). Do you really want to quit? [y/n]: ");
     fflush(stdout);
     c = (char) fgetc(stdin);
     fflush(stdin);
   }
   else
   {
     printf("\nBenchIT: Received SIGINT (Ctrl-C).\n");
     c='y';exitcode=1;
   }
  }
  #ifdef USE_MPI
  MPI_Bcast(&c, 1, MPI_CHAR, 0,MPI_COMM_WORLD);
  #endif
   if (c == 'y' || c == 'Y'){
     if (rank==0)
     {
       printf("BenchIT: Aborting...\n");fflush(stdout);
       analyse_results();
       write_results();
       printf("BenchIT: Finishing...\n");fflush(stdout);
     }
     bi_cleanup(mcb);
     freeall(allresults);
     if (str!=0) free(str);
     if (filename!=0) free(filename);
     if (filename2!=0) free(filename2);
     safe_exit(exitcode);
   }
   else 
   {
     /* read remaining input */
     if (rank==0) while(fgetc(stdin)!='\n');
     /* reinstall handler */
     signal(SIGINT, sigint_handler);
   }
 }

/**
 * Signal handler for SIGTERM, 
 */
static void sigterm_handler (int signum) {

 /* ignore further signals as we are going to quit anyway */
 signal(signum, SIG_IGN);
 if (rank==0)
  {
   fflush(stdout);printf("\nBenchIT: Received SIGTERM, Aborting...\n");fflush(stdout);
   analyse_results();
   write_results(); 
   printf("BenchIT: Finishing...\n");fflush(stdout);
  }
   bi_cleanup(mcb);
   freeall(allresults);
   if (str!=0) free(str);
   if (filename!=0) free(filename);
   if (filename2!=0) free(filename2);
   safe_exit(1);
}


/**
 * Abort function that should be used by the kernels instead of doing an exit(err)
 */
void bi_abort(int err)
{
  if (rank==0)
  {
   fflush(stdout);printf("\nBenchIT: Received SIGTERM, Aborting...\n");fflush(stdout);
   analyse_results();
   write_results(); 
   printf("BenchIT: Finishing...\n");fflush(stdout);
  }
  bi_cleanup(mcb);
  freeall(allresults);
  if (str!=0) free(str);
  if (filename!=0) free(filename);
  if (filename2!=0) free(filename2);
  safe_exit(err);
}

/*!@brief Monstrous main function doing everything BenchIT consists of.
 *
 * This function initializes the kernel, runs the measurements and writes
 * the result-file.
 * @param argc Standard main argument.
 * @param argv Standard main argument.
 * @return 0 on success, >0 on failure.
 */
int main(int argc, char** argv)
{

#ifdef USE_MPI
  /*
  * will contain the number of MPI processes
  */
  int size;
#endif
#ifdef USE_PAPI
  int papi_ver;
#endif
  /*
  * todolist: what problemsizes have to be calculated
  * donelist: what problemsizes have been calculated
  */
  int *todolist=NULL, *donelist=NULL; 
  /*
  * will contain results temporarily of one bi_entry_call for one problemsize
  */
  double *tempresults=NULL;
  /*
  * variables are used for check, how long this process is running
  */
  double totalstart=0, time2=0;
  /*
  * is the timelimit reached?
  */
  int timelimit_reached = 0;
  /* setting infinities */
  BI_INFINITY = pow(2.0,1023.0);
  BI_NEG_INFINITY = pow(-2.0,1023.0);

/*****************************************************************************
 * Initialization
 */
 /* start VAMPIR */
#ifdef VAMPIR_TRACE
  (void) _vptsetup();
  (void) _vptenter(300);
#endif
  /* start MPI */
#ifdef USE_MPI
  IDL(2,printf("MPI_Init()..."));
  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  MPI_Comm_size(MPI_COMM_WORLD,&size);
  IDL(2,printf(" [OK]\n"));
#else
  /* set rank to 0 becaues only one process exists */
  rank=0;
  /* size=1; */
#endif
#ifdef USE_PAPI
  IDL(2,printf("PAPI_library_init()..."));
  papi_ver = PAPI_library_init(PAPI_VER_CURRENT);
  if (papi_ver != PAPI_VER_CURRENT) safe_exit(1);
  IDL(2,printf(" [OK]\n"));
#endif
  /* initialize hashtable for environment variables */
  bi_initTable();
  /* and fill it */
  bi_fillTable();
  /* check the command line arguments for flags */
  checkCommandLine( argc, argv );
  d_bi_start_sec = (double)((long long)bi_gettimeofday());
  /* getting timer granularity and overhead */
  /* these variables can also be accessed by the kernel */
  dTimerOverhead = 0.0;
  dTimerGranularity = 1.0;
  /* select MPI-timer or standard timer or... */
  selectTimer();
  /* only first process shall write this */
  if( rank == 0 )
  {
    printf( "BenchIT: Timer granularity: %.9g ns\n", dTimerGranularity * 1e9 );
    printf( "BenchIT: Timer overhead: %.9g ns\n", dTimerOverhead * 1e9 );
  }
  /* get the time limit */
  p=bi_getenv("BENCHIT_RUN_TIMELIMIT",0);
  /* if the environment variable is set use it */
  if( p != 0 )
    timelimit = atoi(p);
    /* if not, use standard */
  else
    timelimit = DEFAULT_TIMELIMIT;
    /* the same for this environment variable */
  p=bi_getenv("BENCHIT_RUN_ACCURACY",0);
  if( p != 0 )
    accuracy = atoi(p);
  else
    accuracy = DEFAULT_ACCURACY;
  /* prompt info */
  if (rank==0) {printf("BenchIT: Getting info about kernel..."); fflush(stdout);}
  /* fill theinfo with 0s (NOT '0's) */
  (void) memset (&theinfo, 0, sizeof (theinfo));
  /* get info from kernel */
  bi_getinfo(&theinfo);
  /* build infos from the kernelname (also in this file) */
  getKernelNameInfo( &kernelstring, &language, &libraries, &numLibraries );
  /* offset: number of functions from kernel +1 */
  offset=theinfo.numfunctions+1;
  /* print info */
  if (rank==0) {printf(" [OK]\nBenchIT: Getting starting time..."); fflush(stdout);}
  /* starting time used for timelimit */
  totalstart=bi_gettimeofday();
  /* print info */
  if (rank==0) {printf(" [OK]\nBenchIT: Selected kernel: \"%s\"\n",
                       kernelstring != 0
                       ? kernelstring : "NULL");fflush(stdout);}
  /* print info */
  if (rank==0) {printf("BenchIT: Initializing kernel..."); fflush(stdout);}
  /* Backwards compatibility */
  if( theinfo.maxproblemsize != 0 )
    theinfo.num_measurements = theinfo.maxproblemsize;
  /* */
  /* initialize kernel */
  mcb=bi_init(theinfo.num_measurements);
  /* print info */
  if (rank==0) {printf(" [OK]\n"); fflush(stdout);}
  if (rank==0)
  {
    printf("BenchIT: Allocating memory for results..."); fflush(stdout);
    /* all results, which will be measured for one problemsize */
    allresults=(double*)malloc(sizeof(double)*offset*theinfo.num_measurements);
    /* results, which will be measured with a single call of bi_entry */
    tempresults=(double*)malloc(sizeof(double)*offset);
    /* information for the y-axis */
    ydata = (axisdata*)malloc( sizeof(axisdata) * theinfo.numfunctions );
    /* if a malloc didnt work */
    if( (allresults==0) || (tempresults==0) || (ydata==0) )
    {
      printf(" [FAILED]\n");
      printf(" allresults: %lx, tempresults: %lx, ydata: %lx \n", (unsigned long) allresults, (unsigned long) tempresults, (unsigned long) ydata);
      freeall(allresults);
      safe_exit(1);
    }
    /* if it worked */
    else {printf(" [OK]\n"); fflush(stdout);}
    /* fill them with bytes 0 */
    (void) memset( &xdata, 0, sizeof(axisdata) );
    (void) memset( &ydata_global, 0, sizeof(axisdata) );
    (void) memset( ydata, 0, sizeof(axisdata) * theinfo.numfunctions );
    /* standard name for axis */
    xdata.name = 'x';
    ydata_global.name = 'y';
    for( i=0; i<theinfo.numfunctions; ++i )
    ydata[i].name = 'y';
  }
  /* build list for done problemsizes and todo problemsizes */
  todolist=(int*)malloc(sizeof(int)*(theinfo.num_measurements+1));
  donelist=(int*)malloc(sizeof(int)*(theinfo.num_measurements+1));
  /* did malloc work? */
  if ((todolist==0) || (donelist==0))
  {
    printf(" [FAILED]\n");
    freeall(allresults);
    safe_exit(1);
  }
  /* fill it with bytes 0 */
  (void) memset( todolist, 0, sizeof (int)*(theinfo.num_measurements+1) );
  (void) memset( donelist, 0, sizeof (int)*(theinfo.num_measurements+1) );

  /* setup signalhandlers */
  signal(SIGINT,sigint_handler);
  signal(SIGTERM,sigterm_handler);
/*****************************************************************************
 * Measurement
 */
  /* print info */
  if (rank==0) {printf("BenchIT: Measuring...\n"); fflush(stdout);}
  if ((rank==0) && (DEBUGLEVEL==0))
  {
    if ((bi_getenv("BENCHIT_PROGRESS_DIR",0)!=NULL)&&(strcmp(bi_getenv("BENCHIT_PROGRESS_DIR",0),"")))
    {
      int size = strlen(bi_getenv("BENCHITROOT",0))+strlen(bi_getenv("BENCHIT_PROGRESS_DIR",0))+ strlen(bi_getenv("BENCHIT_KERNELNAME",0))+25;
      char* tmp;
      progf=malloc(size);
      memset(progf,0,size);
      progf+=strlen(bi_getenv("BENCHITROOT",0));progf++;
      tmp = progf;

      sprintf(progf,"%s",bi_getenv("BENCHIT_PROGRESS_DIR",0));

      if (progf[0]=='\"') tmp++;

      if (progf[strlen(progf)-1]=='\"')progf[strlen(progf)-1]='\0';
      if (progf[strlen(progf)-1]!='/') progf[strlen(progf)]='/';

      progf+=strlen(progf);
      sprintf(progf,"%s_",bi_getenv("BENCHIT_KERNELNAME",0));
      progf+=strlen(bi_getenv("BENCHIT_KERNELNAME",0))+1;
      sprintf(progf,"%018.6f",bi_gettimeofday());

      if (tmp[0]!='/')
      {
        tmp--;
        tmp[0]='/';
        tmp-=strlen(bi_getenv("BENCHITROOT",0));
        strncpy(tmp,bi_getenv("BENCHITROOT",0),strlen(bi_getenv("BENCHITROOT",0)));
      }

      progf = tmp;

      prog_file = fopen(progf,"w");

      if (prog_file!=NULL)
      {
        printf("BenchIT: writing progress information to file: %s\n",progf);
        fprintf(prog_file,"0%%");fflush(prog_file);
      }
      else printf("could not create file \"%s\" for writing progress information\n",progf);
    }
    printf("progress scale (percent):\n");
    printf("0--------20--------40--------60--------80-------100\n");
    printf("progress:\n");

    fflush(stdout);
  }
  /* as long as there are still some problems (todo-/done-lists can be created) to measure */
  /* and we didnt exceed the timelimit do */
  while( get_new_problems(todolist,donelist,theinfo.num_measurements) && !timelimit_reached )
  {
    /* as long as ther is something in the todolist and the time limit isn't reached do measure */
    v=0;
    while( (todolist[++v] != 0) && !timelimit_reached )
    {
      IDL(2,printf("Testing with problem size %d",todolist[v]));
      /* do accuracy+1 measurement */
      for( w=-1; w<accuracy; w++ )
      {
        /* if MPI is used, set a barrier to synchronize */
#ifdef USE_MPI
        if (theinfo.kernel_execs_mpi1 != 0) MPI_Barrier(MPI_COMM_WORLD);
#endif
        IDL(2,printf(" entering(%d)... ",rank ));
        /* do measurement for non-MPI or first MPI-process */
        if (rank == 0)
          flag=bi_entry(mcb,todolist[v],allresults+offset*(todolist[v]-1));
        else
          if ((theinfo.kernel_execs_mpi1 != 0)||(theinfo.kernel_execs_mpi2 != 0))
            flag=bi_entry(mcb,todolist[v],0);
          else
          {
            printf("\nBenchIT: Warning: Maybe you should check the bi_getinfo funktion\n");
            printf("\nBenchIT:          infostruct->kernel_execs_mpi1 = 0 AND\n");
            printf("\nBenchIT:          infostruct->kernel_execs_mpi2 = 0 ???\n");
          }
        /* for timelimit check */
        time2=bi_gettimeofday();
        IDL(2,printf(" leaving(%d)...", rank));
        /* was sth else then 0 returned? */
        if( flag!=0 )
        {
          /* finalize ;) */
          if( rank==0 )
            printf(" [FAILED]\nBenchIT: Internal kernel error. \n");
          freeall(allresults);
          safe_exit(1);
        }
        /* if everything is fine: */
        /* say: this problemsize is done */
        else donelist[todolist[v]]=1;
        /* only the first one needs to do this */
        if( rank==0 )
        {
          /* here we check, which measurement has been the best for one problemsize, but all fuctions (increment over i) */
          for( i=1; i<offset; i++ )
          {
            /* if it is the first measurement */
            if( w==-1 )
            /* set it to the only measured */
              tempresults[i]=allresults[offset*(todolist[v]-1)+i];
            if ((allresults[offset*(todolist[v]-1)+i] > INVALID_MEASUREMENT)||(allresults[offset*(todolist[v]-1)+i] < INVALID_MEASUREMENT))
            {
              /* if we are looking for the min */
              if (theinfo.outlier_direction_upwards[i-1])
                if (tempresults[i]>allresults[offset*(todolist[v]-1)+i])
                  tempresults[i]=allresults[offset*(todolist[v]-1)+i];
              /* if we are looking for the max */
              if (!(theinfo.outlier_direction_upwards[i-1]))
                if (tempresults[i]<allresults[offset*(todolist[v]-1)+i])
                  tempresults[i]=allresults[offset*(todolist[v]-1)+i];
            }
          }
          /* set the best results */
          tempresults[0]=allresults[offset*(todolist[v]-1)];
          IDL(3,printf("tempresults="));
          for(i=0;i<offset;i++) {IDL(3,printf(" %g",tempresults[i]))}
          IDL(3,printf("\nallresults[%d]=",todolist[v]-1));
          for(i=0;i<offset;i++) {IDL(3,printf(" %g",allresults[offset*(todolist[v]-1)+i]))}
          IDL(3,printf("\n"));
        }
        /* another barrier for synchronization */
#ifdef USE_MPI
        if (theinfo.kernel_execs_mpi1 != 0) MPI_Barrier(MPI_COMM_WORLD);
#endif
        /* timelimit reached? */
        if( (timelimit > 0) && ((time2-totalstart) > timelimit) )
        {
          if (rank==0)
            printf("[BREAK]\nBenchIT: Total time limit reached. Stopping measurement.");
          timelimit_reached = 1;
          break;
        }
      } /* for(w=-1;w<accuracy;w++) */
      IDL(2,printf("...OK\n"));
      /* write progress information to file if available */
      if ((rank==0) && (DEBUGLEVEL==0))
      {
        n++;
        if (50*n/theinfo.num_measurements!=50*(n+1)/theinfo.num_measurements)
        {
          printf(".");
          fflush(stdout);
        }
        if ((100*n/theinfo.num_measurements!=100*(n-1)/theinfo.num_measurements)&&(prog_file!=NULL))
        {
           truncate(progf, 0);
           fprintf(prog_file,"progress: %i%%\n",100*n/theinfo.num_measurements);fflush(prog_file);
        }
      }
      /* only the first process copies the best results to the final results */
      if (rank==0)
      (void)memcpy(allresults+offset*(todolist[v]-1),tempresults,sizeof(double)*offset);
    } /* while (todolist...)*/
  } /* while (get_new_pr...)*/

  if (rank==0)
  {
   printf("\n"); fflush(stdout);
   /*****************************************************************************
    * Analyzing results (Getting Min, Max)
    */
    analyse_results();
   /*****************************************************************************
    * Write *.bit-file (resultfile)
    * and *.bit.gp file (gnuplot file for quickview)
    */
    write_results();
   }   /* rank == 0*/

/*****************************************************************************
 * Finishing
 */
  if (rank==0) {printf("BenchIT: Finishing...\n"); fflush(stdout);}
  bi_cleanup(mcb);
  freeall(allresults);
  if (str!=0) free(str);
  if (filename!=0) free(filename);
  if (filename2!=0) free(filename2);
  safe_exit(0);
  exit(0); /*to eliminate compiler-warning */
}


/*!@brief Create the directories of the pathname denoted by dirstr.
 *
 * If any part of the pathname is not accessible or can not be createad, the
 * function will safe_exit().
 * @param dirstr The pathname which shall be created.
 */
/* creates directory structure or exits if that's not possible */
static void createDirStructureOrExit( const char *dirstr )
{
  /* buffer for string work */
  char buf[100];
  int flag = 0, i = 0, len = length( dirstr ) + 1, pos = -1, last = 0;
  int successFlag = 1;
  /* set the content of the buffer to 000000... */
  memset( buf, 0, 100 );
  /* if the directory starts with an / (is the root-directory) */
  if ( dirstr[0] == '/' )
  {
    /* start at / and go through all subdirs one by one */
    while ( last >= 0 )
    {
      /* char-position after the actual subfolders name */
      i = indexOf( dirstr, '/', last );
      pos = i;
      /* if there is no ending / */
      if ( pos < 0 ) pos = len - 1;
      /* get actual subfolders name and write it to buf */
      substring( dirstr, buf, last, pos + 1 );
      /* try to change into the subdir */
      flag = chdir( buf );
      if ( flag != 0 )
      {
        /* if it doesn't exist, try to create it */
        mkdir( buf, S_IRWXU );
        /* if it couldnt be created */
        if ( chdir( buf ) != 0 )
        {
          successFlag = 0;
        }
      }
      /* no more '/'s found */
      if ( i < 0 ) last = -1;
      /* more found, go to the next subfolder */
      else last = pos + 1;
    }
  }
  /* couldn't create subfolder? */
  if ( successFlag != 1 )
  {
    printf(" [FAILED]\nBenchIT: could not create directory structure \"%s\"\n",dirstr);
    safe_exit(127);
  }
}

/*!@brief Returns the elapsed time since the "epoch" (1/1/1970) in seconds
 *
 * This function is just a wrapper which combines the two integers of the
 * timeval struct to a double value.
 * @return The elapsed time since the epoch in seconds.
 */
static double bi_gettimeofday(void)
{
  struct timeval time;
  gettimeofday( &time, (struct timezone *) 0);
  return (double)time.tv_sec + (double)time.tv_usec * 1.0e-6;
}

/*!@brief Returns the elapsed time since program start in seconds.
 *
 * This function has improved precision over bi_gettimeofday(), because the
 * amount of seconds is smaller and that leaves more space for the fractional
 * part of the double value.
 * @return The elapsed time since program start in seconds.
 */
static double bi_gettimeofday_improved(void)
{
  struct timeval time;
  gettimeofday( &time, (struct timezone *) 0);
  return (double)(time.tv_sec - d_bi_start_sec) + (double)time.tv_usec * 1.0e-6;
}

/*!@brief Determines the granularity of a given timer function.
 * @param[in] timer Pointer to the timer function that shall be evaluated.
 * @return The minimum time in seconds that the given timer function can
 *         distinguish.
 * @TODO: you could add additional timers here but you should also add it to getTimerOverhead/selectTimer!
 */
static double getTimerGranularity( double (*timer)() )
{
  int i;
  double t1, t2, gran = 1.0;
  /* first time */
  t2 = timer();
  /* do thousand times for more exact measurement */
  for (i = 1; i < 10000; i++)
  {
    /* new timer set to old timer */
    t1 = t2;
    /* while the returned value is the same as the old one */
    while( t1 >= t2 )
      t2 = timer();
    /* if the step between the old and the new time is the lowest ever, set it as granularity.
       Unfortunately floating point subtraction can produce results !=0 when subtracting two identical
       numbers, to filter senseless values (like 1.0e-10 ns) we check if the result is within the range
       of double relativly to the greater operand t2*/
    if( ((t2 - t1) < gran)&&((t2-t1)>t2*1.0e-15) )
      gran = t2 - t1;
  }
  /* found timer */
  if( timer == bi_gettimeofday_improved )
  {
  }

#ifdef USE_PAPI
  /* found timer */
  else if( timer == PAPI_gettime )
  {
  }
#endif

#ifdef USE_OMP
  /* found timer and set the granularity to the one, which is given by the OpenMP-lib */
  else if( timer == omp_get_wtime )
  {
    gran = omp_get_wtick();
  }
#endif

#ifdef USE_MPI
  /* found timer and set the granularity to the one, which is given by the mpi-lib */
  else if( timer == MPI_Wtime )
  {
    gran = MPI_Wtick();
  }
#endif
  /* unsupported timer */
  else
  {
    (void) fprintf (stderr, "BenchIT: getTimerGranularity(): Unknown timer\n");
    (void) fflush (stderr);
    (void) safe_exit (127);
  }
  return gran;
}

/*!@brief Determines the overhead of a call to a given timer function.
 * @param[in] timer Pointer to the timer function that shall be evaluated.
 * @return The overhead in seconds of one call to the timer function.
 * @TODO: you could add additional timers here but you should also add it to getTimerGranularity/selectTimer!
 */
static double getTimerOverhead( double (*timer)() )
{
  double start, stop, diff;
  int passes = 1000;
  int i;

  do
  {
    passes *= 10;
    start = timer();
    for( i = 0; i < passes; ++i )
      (void)timer();
    stop = timer();
    diff = stop - start;
  } while( diff < 1000 * dTimerGranularity );
  IDL(3,printf( "getTimerOverhead: %i passes\n", passes ));

  return diff / passes;
}

#ifdef USE_PAPI
static double PAPI_gettime(void)
{
  return (double) PAPI_get_real_usec() *1.0e-6;
}
#endif


/*!@brief Determine and select the best timer function.
 *
 * Determines the most precise available timer function and selects it for
 * use in the kernels via the bi_gettime() function.\n
 * Currently supported are gettimeofday() and MPI_Wtime().
 * @TODO: you could add additional timers here but you should also add it to getTimerGranularity/overhead!
 */
static void selectTimer()
{
  /* the timer to use
   * 0 -> use bi_gettimeofday_improved() [default]
   * 1 -> use PAPI-Timer   (PAPI_gettime()) based on PAPI_get_real_usec()
   * 2 -> use OpenMP-Timer (omp_get_wtime())
   * 3 -> use MPI-Timer    (MPI_WTime())
   */
  int select;

#ifdef USE_MPI
 int mpiRoot = 0, mpiRank;
 /* get own rank */
 MPI_Comm_rank(MPI_COMM_WORLD,&mpiRank);
 /* only the master should do this */
 if( mpiRank == 0 )
  {
#endif

    double granularity;

    /* setup default timer */
    granularity = getTimerGranularity( bi_gettimeofday_improved );
    IDL(1,printf("BenchIT: Timer Granularity: bi_gettimeofday_improved: %.9g ns\n", granularity * 1.0e9));
    select = 0;
    dTimerGranularity = granularity;
    bi_gettime = bi_gettimeofday_improved;
    /* get overhead */
    dTimerOverhead = getTimerOverhead( bi_gettime );

    /* testing if another timer has a better granularity */
#ifdef USE_PAPI
    /* get granularity of PAPI-timer */
    granularity = getTimerGranularity( PAPI_gettime );
    IDL(1,printf("BenchIT: Timer Granularity:       PAPI_get_real_usec: %.9g ns\n", granularity * 1.0e9));
    if( granularity < dTimerGranularity )
    {
      select = 1;
      dTimerGranularity = granularity;
      bi_gettime = PAPI_gettime;
      /* get overhead */
      dTimerOverhead = getTimerOverhead( bi_gettime );
    }
#endif

#ifdef USE_OMP
    /* get granularity of OpenMP-timer */
    granularity = getTimerGranularity( omp_get_wtime );
    IDL(1,printf("BenchIT: Timer Granularity:            omp_get_wtime: %.9g ns\n", granularity * 1.0e9));
    if( granularity < dTimerGranularity )
    {
      select = 2;
      dTimerGranularity = granularity;
      bi_gettime = omp_get_wtime;
      /* get overhead */
      dTimerOverhead = getTimerOverhead( bi_gettime );
    }
#endif

#ifdef USE_MPI
    /* get granularity of MPI-timer */
    granularity = getTimerGranularity( MPI_Wtime );
    IDL(1,printf("BenchIT: Timer Granularity:                MPI_Wtime: %.9g ns\n", granularity * 1.0e9));
    /* select MPI-Timer if it is faster */
    if( granularity < dTimerGranularity )
    {
      select = 3;
      dTimerGranularity = granularity;
      bi_gettime = MPI_Wtime;
      /* get overhead */
      dTimerOverhead = getTimerOverhead( bi_gettime );
    }
  }
  /* send this timer settings to all other nodes */
  MPI_Bcast(&select, 1, MPI_INT, mpiRoot,MPI_COMM_WORLD);
  MPI_Bcast(&dTimerGranularity, 1, MPI_DOUBLE,mpiRoot,MPI_COMM_WORLD);
  MPI_Bcast(&dTimerOverhead, 1, MPI_DOUBLE,mpiRoot,MPI_COMM_WORLD);
  /* select timer in other nodes */
  switch( select )
  {
  case 0:
    bi_gettime = bi_gettimeofday_improved;
    if (mpiRank == 0) printf("BenchIT: Using Timer \"bi_gettimeofday_improved\"\n");
    break;
  case 1:
    #ifdef USE_PAPI
    bi_gettime = PAPI_gettime;
    if (mpiRank == 0) printf("BenchIT: Using Timer \"PAPI_get_real_usec\"\n");
    break;
    #endif
  case 2:
    #ifdef USE_OMP
    bi_gettime = omp_get_wtime;
    if (mpiRank == 0) printf("BenchIT: Using Timer \"omp_get_wtime\"\n");
    break;
    #endif
  case 3:
    bi_gettime = MPI_Wtime;
    if (mpiRank == 0) printf("BenchIT: Using Timer \"MPI_Wtime\"\n");
    break;
  default:
    (void) fprintf (stderr, "BenchIT: selectTimer(): Unknown timer\n");
    (void) fflush (stderr);
    (void) safe_exit (127);
  }
#else
  switch( select )
  {
  case 0:
    printf("BenchIT: Using Timer \"bi_gettimeofday_improved\"\n");
    break;
  case 1:
    printf("BenchIT: Using Timer \"PAPI_get_real_usec\"\n");
    break;
  case 2:
    printf("BenchIT: Using Timer \"omp_get_wtime\"\n");
    break;
  default:
    fprintf (stderr, "BenchIT: selectTimer(): Unknown timer\n");
    fflush (stderr);
    safe_exit (127); 
  }
#endif
}

/*!@brief Duplicates a given string.
 * @param[in] str The string that shall be copied.
 * @return Pointer to the copy of the string.
 */
char* bi_strdup(const char *str)
{
  /* incoming */
  const char *pi;
  /* work and outgoing */
  char *so, *po;
  /* NULL given as str */
  if (str == 0)
  {
    (void) fprintf (stderr, "BenchIT: bi_strdup(): NULL as argument\n");
    (void) fflush (stderr);
    (void) safe_exit (127);
  }
  /* set incoming */
  pi = str;
  /* find ending flag 0, move the pi-pointer there */
  while (*pi != '\0')
  pi++;
  /* create a new string with the length of */
  /* (ending-address of str)+1-(beginnig adress of str) */
  /* which means the length of the exact string (not counting chars after first '\0') */
  so = (char*)malloc ((size_t) (pi + 1 - str) * sizeof (char));
  /* if the space for this copy isn't avail or sth else failed */
  if (so == 0)
  {
    (void) fprintf (stderr, "BenchIT: bi_strdup(): No more core\n");
    (void) fflush (stderr);
    (void) safe_exit (127);
  }
  /* input for this work is again string, output will be so, po is the pointer to so */
  pi = str;
  po = so;
  /* until we find ending flag ... */
  while (*pi != '\0')
    /* ... we copy the next char in string */
    *po++ = *pi++;
  /* we copy the \0 also */
  *po = *pi;
  /* and return the whole copy */
  return (so);
}

/*!@brief Reads and copies environment variable with the name supplied in env.
 *
 * If the environment variable ist not defined, the value of exitOnNull
 * determines the behaviour of this function:\n
 * @li @c exitOnNull @c = @c 0: The return value will be @c NULL.
 * @li @c exitOnNull @c = @c 1: Exit BenchIT with an error message.
 * @li @c exitOnNull @c > @c 1: Exit BenchIT with an error message and additionally dump
 *     the environment variable hashtable.
 *
 * @param[in] env The name of the environment variable whose content shall be retrieved.
 * @param[in] exitOnNull Defines how to handle errors.
 * @return Pointer to a copy of the environment variable.
 */
char* bi_getenv(const char *env, int exitOnNull)
{
  char *tmp, *res = 0;
  int l = -1;
  /* First try to read from Environment*/
  res = getenv(env);
  if (res!=NULL) return res;

  /* If not found look up environment variable in the hashtable. */
  tmp = bi_get( env, &l );
  if ( tmp != 0 )
  {
    res = bi_strdup( tmp );
  }
  else if ( exitOnNull > 0 )
  {
    (void) fprintf (stderr, "BenchIT: bi_getenv(): env. variable %s not defined\n", env);
    (void) fflush (stderr);
    if ( exitOnNull > 1 ) bi_dumpTable();
    (void) safe_exit (127);
  }
  return res;
}

/*!@brief Reads and converts environment variable with the name supplied in env
 *        to a long int.
 *
 * If the environment variable ist not defined or if there is an error
 * converting the string to an integer value, the value of exitOnNull determines
 * the behaviour of this function:\n
 * @li @c exitOnNull @c = @c 0: The return value will be @c 0.
 * @li @c exitOnNull @c = @c 1: Exit BenchIT with an error message.
 * @li @c exitOnNull @c > @c 1: Exit BenchIT with an error message and
 *        additionally dump the environment variable hashtable.
 *
 * @param[in] env The name of the environment variable whose content shall be
 *                retrieved.
 * @param[in] exitOnNull Defines how to handle errors.
 * @return Value of env as long int.
 */
long int bi_int_getenv(const char *env, int exitOnNull)
{
  /* used for getting environment as string */
  const char *string = 0;
  /* end of the string */
  char *endptr = 0;
  /* what is computed out of the string */
  long int result = 0;
  /* was there an error? */
  int error = 0;
  /* get the environment variable to string */
  string = bi_getenv (env, exitOnNull );
  /* found? */
  if ( *string != 0 )
  {
    /* get the numereous meaning (ending char is a 0 (args)) and a decimal meaning (arg3) */
    result = strtol (string, &endptr, 10);
    /* couldnt translate? */
    if ( *endptr != 0 ) error++;
  }
  /* couldnt get env? */
  else error++;
  /* error? */
  if ( error > 0 )
  {
    if ( exitOnNull > 0 )
    {
      (void) fprintf (stderr, "BenchIT: bi_int_getenv(): env. variable %s not an int (%s)\n",
                      env, string);
      (void) fflush (stderr);
      if ( exitOnNull > 1 ) bi_dumpTable();
      (void) safe_exit (127);
    }
    result = 0;
  }
  /* error or not */
  return result;
}

/*! translates a string to a fractional part
 * @param[in] r a string, which numbers are after '.', including the '.'\
  (which could be any char, though it isn't checked ;))
 * @return the meaning of the String as float
 * COMMENT: Example: In: String ".12345", Out: float 0.12345
 */
static float fracpart(char *r)
{
  /* a is total sum, s is actual decimal position */
  float a=0, s=1;
  do
  {
    /* start with 1/10, then 1/100, ... */
    s=s/10;
    /* next char */
    r++;;
    /* if it is a number between 0 and 9 */
    if (('0' <= *r) && (*r <='9'))
    {
      /* add this part to total sum */
      a+=s* (*r - '0');
    }
    /* until this char isn't a number anymore */
  } while(('0' <= *r) && (*r <='9'));
  /* return total fractional part */
  return a;
}

/*!@brief Converts $BENCHIT_ARCH_SPEED into float [GHz]
 * @return Clock rate in GHz as float.
 * COMMENT: be careful with this function! The user could write anything in his BENCHIT_ARCH_SPEED
 */
float bi_cpu_freq(void)
{ /* get CPU-Freq in GHz */
  char *p,*q;
  /* return value */
  float f;
  /* get the speed (e.g. 3G3 or 200M) */
  p=bi_getenv("BENCHIT_ARCH_SPEED",0);
  /* wasn't set */
  if (p == NULL)
  return 0.0;
  /* try to translate it to float (will get everything, until the G) */
  f = (float) atof(p);
  /* find G */
  q = strstr( p,"G");
  /* not found? find g */
  if (q == NULL) q = strstr( p,"g");
  /* found? */
  if (q!=NULL)
  {
    /* add the value before the g and the fractional part and return it */
    return f+fracpart(q);
  }
  /* maybe its an M */
  q = strstr( p,"M");
  /* or m */
  if (q == NULL) q = strstr( p,"m");
  /* do the same blabla */
  if (q!=NULL)
  {
    return (f+fracpart(q))/1000;
  }
  return f;
}

/*!@brief Print contents of an axisdata struct for debugging purposes.
 * @param[in] ad Pointer to an axisdata structure.
 */
static void print_axisdata( const axisdata *ad )
{
  printf( "\nMin: %.20e\nMax: %.20e\nDiff: %.20e\nPlotmin: %e\nPlotmax: %e\n", ad->min, ad->max, ad->max-ad->min, ad->plotmin, ad->plotmax );
  printf( "Base: %e\nTicks: %i\nIncr: %e\n", ad->base, ad->ticks, ad->incr );
  fflush( stdout );
}

/*!@brief Compute values for use by gnuplot.
 *
 * This function computes nice values for the generation of the gnuplot file.\n
 * It needs ad->min, ad->max and ad->base set.\n
 * The plotmin, plotmax, ticks and incr members of ad will be set.
 * @param[in,out] ad Pointer to an axisdata struct.
 */
static void get_axis_properties( axisdata *ad )
{
  if( DEBUGLEVEL >= 2 )
    print_axisdata( ad );
  if(( ad->base >= 0 )&&( ad->base <= 0 ))
  { /* linear scale */
    double diff, tickspacing;
    /* difference between min and max */
    diff = ad->max - ad->min;
    if( diff <= 1e-30 )
      return;
    /* minimal possible value for each axis is zero */
    ad->plotmin = 0.0;
    /* start with ticks like 1,2,3,4,5,6... */
    tickspacing = 1.0;
    /* if the difference is larger then 10 */
    if( diff > 10.0 )
    {
      /* do ticks like 50,60,70,80 or 500,600,700,800 or according to difference */
      while( diff > 10.0 )
      {
        diff /= 10.0;
        tickspacing *= 10.0;
      }
    }
    else
    {
      /* or ticks at .3,.4,.5... or less */
      while( diff < 1.0 )
      {
        diff *= 10.0;
        tickspacing /= 10.0;
      }
    }
    /* use ticks at .25 , .5 , .75 */
    if( diff <= 2.5 )
    {
      tickspacing /= 4.0;
    }
    else if( diff <= 5.0 )
    {
      tickspacing /= 2.0;
    }
    /* set incr. */
    ad->incr = tickspacing;
    /* find the plot-min (should be a tick) */
    while( ad->plotmin <= (ad->min - tickspacing) )
    ad->plotmin += tickspacing;
    /* set the max to min before finding max (incrementing, until its larger then the max measured value) */
    ad->plotmax = ad->plotmin;
    /* first, we have no ticks */
    ad->ticks = 0;
    /* find max */
    while( ad->plotmax < ad->max )
    {
      ad->plotmax += tickspacing;
      ad->ticks += 1;
    }
  }
  else
  { /* logarithmic scale */
    /* be sure that it is displayable */
    if( ad->min <= 0.0 )
    {
      (void) fprintf (stderr, "BenchIT: The minimum value of a result of your kernel is equal to or\n");
      (void) fprintf (stderr, "         smaller than 0.0, but logarithmic scaling was requested, which\n");
      (void) fprintf (stderr, "         is impossible for values <= 0.0\n");
      (void) fprintf (stderr, "         This is a bug in the kernel, contact its developer please.\n");
      (void) fflush (stderr);
      (void) safe_exit (127);
    }
    /* find min ... */
    ad->plotmin = ad->base;
    /* for mins smaller then the base */
    if( ad->plotmin > ad->min )
    {
      while( ad->plotmin > ad->min )
      ad->plotmin /= ad->base;
    }
    /* or mins larger then the base */
    else
    {
      while( ad->plotmin <= (ad->min / ad->base) )
      ad->plotmin *= ad->base;
    }
    /* set number of ticks and max's */
    ad->ticks = 0;
    ad->incr = ad->base;
    ad->plotmax = ad->plotmin;
    /* find max, which is a number like base^n and is larger then the real max */
    /* set the ticks accordingly */
    while( ad->plotmax < ad->max )
    {
      ad->plotmax *= ad->base;
      ad->ticks += 1;
    }
    /* remove all ticks, which are more then 10 */
    while( ad->ticks > 10 )
    {
      if( ad->ticks & 0x1 )
        ad->ticks++;
      ad->ticks /= 2;
      ad->incr *= ad->incr;
    }
  }
  if( DEBUGLEVEL >= 2 )
    print_axisdata( ad );
}

/*!@brief Determine the proper prefix index and scaling for the given value.
 *
 * @param[in] value The number for which the scaling shall be computed.
 * @param[out] scaling_level Index for BIN_Pref or SI_PREF\n
 *             Positive for k, M, G, etc., negative for m, u, etc.
 * @param[out] scaling_value Multiply value with this number to get the proper
 *             scaled number for the prefix.
 * @param[in] bBase2Scaling Boolean value to select the use of the scaling based
 *            on 1024 (@c bBase2Scaling<>0) rather than 1000 (@c bBase2Scaling=0).
 */
static void get_scaling( double value, int * scaling_level, double * scaling_value, int bBase2Scaling )
{
  /* decimal scaling */
  double scaling_base = 1000.0;
  /* binary scaling */
  if( bBase2Scaling )
    scaling_base = 1024.0;
  /* first no scale active */
  *scaling_level = 0;
  *scaling_value = 1.0;
  /* no scaling wanted */
  if(( value >= 0 )&&( value <= 0 ))
    return;
  /* <0? get absolute value */
  if( value < 0 )
    value = fabs( value );
  /* find best scaling level by going through data */
  if( value >= scaling_base )
  {
    while( value >= scaling_base )
    {
      value = (float)(value / scaling_base);
      *scaling_level += 1;
    }
  }
  else if( (value < 1.0) && (bBase2Scaling == 0) )
  { /* "negative" scaling not possible for base 2 */
    while( value < 1.0 )
    {
      value = (float)(value * scaling_base);
      *scaling_level -= 1;
    }
  }
  /* set to max or min possibble scaling */
  if( bBase2Scaling )
  {
    if( *scaling_level > BIN_PREF_MAX )
      *scaling_level = BIN_PREF_MAX;
  }
  else
  {
    if( *scaling_level > SI_PREF_MAX )
      *scaling_level = SI_PREF_MAX;
    else if( *scaling_level < SI_PREF_MIN )
      *scaling_level = SI_PREF_MIN;
  }
  /* scale it */
  *scaling_value = pow( scaling_base, (*scaling_level) * (-1) );
}

/*!@brief Generate settings for gnuplot from an axisdata struct and write to
 *        an already opened file.
 *
 * This function generates the settings for a pretty output from gnuplot.\n
 * It applies size prefixes for the values on the axes and makes ticks on
 * 'nice' positions.\n
 * The settings will be appended to the file denoted by f.
 * @param[in] f An open FILE* with permission to write.
 * @param[in] ad An axisdata struct which was already processed by
 *            get_axis_properties()
 */
static void write_gnuplot_axisproperties( FILE *f, const axisdata *ad )
{
  double pos, pos1, scaling_value;
  int count, i, first = 1;
  int have_scaling = 0;

  switch( (int)ad->base )
  {
  case 0:
    { /* linear scale */
      fprintf( f, "set %ctics (", ad->name );
      if( (ad->plotmin < 0) && (ad->plotmax > 0) )
      { /* search for value nearest to 0 for determining the scaling */
        double dist = 1e30;
        for( pos = ad->plotmin; pos <= ad->plotmax; pos += ad->incr )
        {
          if( fabs( pos ) < dist )
          dist = pos;
        }
        get_scaling( dist, &count, &scaling_value, 0 );
        have_scaling = 1;
      }
      for( pos = ad->plotmin; pos <= ad->plotmax; pos += ad->incr )
      {
        /*print_axisdata( ad ); */
        if( !have_scaling )
        {
          get_scaling( pos, &count, &scaling_value, 0 );
          if(( pos > 0.0 )||( pos < 0.0 ))
          have_scaling = 1;
        }
        pos1 = pos * scaling_value;
        if( first )
        {
          fprintf( f, "\"%.6g%c\" %e", pos1, SI_pref[count], pos );
          first = 0;
        }
        else
        {
          fprintf( f, ",\"%.6g%c\" %e", pos1, SI_pref[count], pos );
        }
      }
      fprintf( f, ")\n" );
      break;
    }
  case 2:
    {
      fprintf( f, "set %ctics (", ad->name );
      for( pos = ad->plotmin; pos <= ad->plotmax; pos *= ad->incr )
      {
        if( !have_scaling )
        {
          get_scaling( pos, &count, &scaling_value, 1 );
          if(( pos > 0.0 )||( pos < 0.0 ))
          have_scaling = 1;
        }
        pos1 = pos * scaling_value;
        if( first )
        {
          fprintf( f, "\"%.6g%s\" %e", pos1, BIN_pref[count], pos );
          first = 0;
        }
        else
        fprintf( f, ",\"%.6g%s\" %e", pos1, BIN_pref[count], pos );
      }
      fprintf( f, ")\n" );
      fprintf( f, "set logscale %c %g\n", ad->name, ad->base );
      break;
    }
  case 10:
    {
      fprintf( f, "set %ctics (", ad->name );
      for( pos = ad->plotmin; pos <= ad->plotmax; pos *= ad->incr )
      {
        get_scaling( pos, &count, &scaling_value, 0 );
        pos1 = pos * scaling_value;
        if( first )
        {
          fprintf( f, "\"%.6g%c\" %e", pos1, SI_pref[count], pos );
          first = 0;
        }
        else
        {
          fprintf( f, ",\"%.6g%c\" %e", pos1, SI_pref[count], pos );
        }
        if( ad->ticks <= 5 )
        for( i=2; i<=9; ++i )
        fprintf( f, ",\"\" %e 1", pos * i );
      }
      fprintf( f, ")\n" );
      fprintf( f, "set logscale %c %g\n", ad->name, ad->base );
      break;
    }
  default:
    fprintf( f, "set %ctics %e,%e\n", ad->name, ad->plotmin, ad->incr );
    break;
  }
  fprintf( f, "set %crange [%e:%e]\n", ad->name, ad->plotmin, ad->plotmax );
}

/**
* used to print information about compiler to file
*/
static void print_C_Compiler_information_to_file(FILE * bi_out,char* buf)
{
/* This should work on all compilers except Intel  */
#ifdef __VERSION__
  /* which needs those */
  #ifdef __INTEL_COMPILER
    #ifdef __INTEL_COMPILER_BUILD_DATE
      sprintf(buf,"c_compiler_version=\"%i, build %i\"\n",__INTEL_COMPILER,__INTEL_COMPILER_BUILD_DATE);
      bi_fprintf(bi_out,buf);
      IDL(1,printf("print_C_Compiler_information_to_file: case %i\n",1));
    #else
      sprintf(buf,"c_compiler_version=\"%s\"\n",__INTEL_COMPILER);
      bi_fprintf(bi_out,buf);
      IDL(1,printf("print_C_Compiler_information_to_file: case %i\n",2));
    #endif
  #else
    sprintf(buf,"c_compiler_version=\"%s\"\n",__VERSION__);
    bi_fprintf(bi_out,buf);
    IDL(1,printf("print_C_Compiler_information_to_file: case %i\n",3));
  #endif
#endif
}

/*!@brief Tries to confuse the Cache by filling nCacheSize bytes with
 * data and calculating with it
 * @param[in] nCacheSize number of bytes to allocate
 *            (should be a multiple of sizeof(int))
 * @returns a number which can be ignored ;)
 */
int bi_confuseCache(int nCacheSize)
{
/* trying to fill the L2-cache with uninteristing stuff */
  int s=0, i,*memConfuse;

  if (nCacheSize == 0)
    return 1;
  memConfuse = (int*)malloc(nCacheSize );
  nCacheSize = nCacheSize/sizeof(int);
  for (i=0; i<nCacheSize; memConfuse[i++]=1);
  for (i = nCacheSize/2; i < nCacheSize; i++)
    s += memConfuse[i]+memConfuse[i-nCacheSize/2];
  for (i = nCacheSize/2; i < nCacheSize; i++)
    s += memConfuse[i]+memConfuse[i-nCacheSize/2];
  for (i = nCacheSize/2; i < nCacheSize; i++)
    s += memConfuse[i]+memConfuse[i-nCacheSize/2];
  for (i = nCacheSize/2; i < nCacheSize; i++)
    s += memConfuse[i]+memConfuse[i-nCacheSize/2];
  free(memConfuse);
  return s;
}

/*!@brief frees result.
 * @param[in] results a double* to free
 */
static void freeall(double* results)
{
  if (results)
    free(results);
}

/*!@brief prints a string to File
 * @param[in] f a FILE* to write a string to
 * @param[in] s a char* to write into f
 */
static void bi_fprintf(FILE *f,char *s)
{
  int a=0,i=0,j=0,k=0;
  char buf[81];
  /* sth doesnt exist? */
  if (f==0) return;
  if (s==0) return;
  /* get length */
  a=(int)strlen(s);
  /* length <=79: write line */
  if (a<=79)
  fprintf(f, "%s", s);
  /* else line shall be broken */
  else
  {
    for(i=0;i<a;i++)
    {
      if (s[i]=='\t') s[i]=' ';
      if ((s[i]=='\\') || (s[i]=='\n'))
      { /* manual line break */
        buf[j]=s[i];
        if (s[i]=='\n') buf[j+1]='\0';
        else
        {
          buf[j+1]='\n';
          buf[j+2]='\0';
        }
        fprintf(f, "%s", buf);
        j=0;
        k=0;
        continue;
      }
      if (s[i]==' ') /* remember last space */
      k=j;
      if (j==79)
      { /* eol reached */
        if (k==0) k=j;
        buf[k]='\\';
        buf[k+1]='\n';
        buf[k+2]='\0';
        fprintf(f, "%s", buf);
        i=i-(j-k);
        j=0;
        k=0;
        continue;
      }
      buf[j]=s[i]; /* copy char */
      j++;
    }
  }
}

/* variables for random number generator */

/*! user defined maximum size of output */
static unsigned long long random_max32,random_max48;

/*!
 * The random number generator uses 2 independent generators and returns the bitwise xor of them
 * both generators use this formula: r(n+1) = ((a * r(n)) +b) mod m
 * the parameters are defined in the bi_random_init() function
 */

/*! parameters for the first generator*/
static unsigned long long random_value1=0;
static unsigned long long rand_a1=0;
static unsigned long long rand_b1=0;
static unsigned long long rand_m1=1;
static unsigned long long rand_fix1=0;

/*! parameters for the second generator */
static unsigned long long random_value2=0;
static unsigned long long rand_a2=0;
static unsigned long long rand_b2=0;
static unsigned long long rand_m2=1;
static unsigned long long rand_fix2=0;

/* end variables for random number generator */

/*! @brief returns a 32-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48() share one state so a call to
 *  bi_random32() will affect the next result of bi_random48() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
unsigned int bi_random32(void)
{
  random_value1 = (random_value1 * rand_a1 + rand_b1)%rand_m1;
  random_value2 = (random_value2 * rand_a2 + rand_b2)%rand_m2;
  return (unsigned int) (random_value1^random_value2) % random_max32;
}

/*! @brief returns a 48-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48()share one state so a call to
 *  bi_random48() will affect the next result of bi_random32() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
unsigned long long bi_random48(void)
{
  random_value1 = (random_value1 * rand_a1 + rand_b1)%rand_m1;
  random_value2 = (random_value2 * rand_a2 + rand_b2)%rand_m2;
  return (random_value1^random_value2) % random_max48;
}

/*! @brief initalizes random number generator
 *  Initializes tehe random number generator with the values given to the function.
 *  The random number generator uses 2 independent generators and returns the bitwise xor of them.
 *  both generators use this formula: r(n+1) = ((a * r(n)) +b) mod m.
 *  @param[in] start start value for random number generation
 *  @param[in] max the generator will allways return numbers smaller than max
 *                 if max is 0 bi_random32 will return numbers between 0 and 2^32 -1
 *                             bi_random48 will return numbers between 0 and 2^48 -1
 */
void bi_random_init(unsigned long long start,unsigned long long max)
{
  /* setting up parameters (direct assignment of long long values causes compiler warning) */
  rand_a1 = atoll("25799");
  rand_b1 = atoll("76546423423");
  rand_m1 = atoll("568563987265559");
  rand_fix1 = atoll("298651465807007");

  rand_a2 = atoll("131");
  rand_b2 = atoll("91723615256891");
  rand_m2 = atoll("338563987265599");
  rand_fix2 = atoll("283167315359180");

  /* setup the max values returned to user*/
  random_max32 = ((unsigned long long) 1) << 32;
  random_max48 = ((unsigned long long) 1) << 48;
  if (max>0)
  {
    if (max < random_max32) random_max32 = max;
    if (max < random_max48) random_max48 = max;
  }

  /* the first generator is initialized with the user defined start value */
  random_value1 = start%rand_m1;
  if (random_value1==rand_fix1) random_value1=atoll("43277143270890");  /* Fixpoint can't be used */

  /* the second generator is initialized with the first random number generated by the first generator*/
  random_value2 = bi_random48();
  if (random_value2==rand_fix2) random_value2 = atoll("678157495234");  /* Fixpoint can't be used */
}

/*! @brief The function parses a list of numbers in a certain sysntax 
 * and returns a chained list with the expanded numbers.
 *  @param[out] count holds the number of elements in the result-list
 *  @param[in] pcstring the string containing the intervalls 
 *  @return expanded list of values which is count elements long
 */
bi_list_t *bi_parselist(unsigned long long *picount, const char *pcstring) {
  /*pointer to the 1st element of the bi_list_t and 
   return value if the function*/
  bi_list_t *pfirst;
  bi_list_t *panchor;
  /*variable for buffering and working*/
  bi_list_t *pelement;
  /*loop variables, variables for memorising
   series of numbers and raise*/
  unsigned long long li, lj, ln, lstartnumber, lendnumber, lraise;

  /*debugging level 1: mark begin and end of function*/
  if (DEBUGLEVEL > 0) {
    printf("reached function parser\n");
    fflush(stdout);
  }

  /*initializing*/
  li = (unsigned long long) 0;
  lj = (unsigned long long) 0;
  ln = (unsigned long long) 0;
  lstartnumber = (unsigned long long) 0;
  lendnumber = (unsigned long long) 0;
  lraise = (unsigned long long) 0;
  *picount = (unsigned long long) 0;
  pfirst = NULL;
  panchor = NULL;

  /*as long as the strings end is not reached do ...*/
  while (pcstring[li] != 0) {
    /*if the beginning of a number is found ...*/
    if (isdigit(pcstring[li]) ) {
      /*save the number that was found*/
      sscanf( (pcstring + li ), "%llu", &lstartnumber);
      /*move ahead in the string until the
       end of the number ...*/
      ln = ( long ) (log10 ( ( double ) lstartnumber ) );
      li += ln + 1;
      /*whitespaces are ignored*/
      if (isspace(pcstring[li]) )
        li++;
      /*if next character is a minus
       -> series of numbers is defined*/
      if (pcstring[li] == '-') {
        li++;
        /*whitespaces are ignored*/
        if (isspace(pcstring[li]) )
          li++;
        /*if next number if found ...*/
        if (isdigit(pcstring[li]) ) {
          /*the number is used as
           the end of the series*/
          sscanf( (pcstring + li ), "%llu", &lendnumber );
          /*move ahead in the string until the
           end of the number*/
          ln = ( long ) (log10 ( (double) (lendnumber ) ) );
          li += ln + 1;

          /*if there is nothing different defined
           all numbers between start and and are
           added to the list*/
          lraise = 1;
          /*whitespaces are ignored*/
          if (isspace(pcstring[li]) )
            li++;
          /*if next char is a slash
           -> raise must be changed to ...*/
          if (pcstring[li] == '/') {
            li++;
            /*whitespaces are ignored*/
            if (isspace(pcstring[li]) )
              li++;
            /*... the following number ...*/
            if (isdigit(pcstring[li]) ) {
              sscanf( (pcstring + li ), "%llu", &lraise );
            }
            /*and it needs to be moved ahead
             until the end of the number*/
            ln = ( long ) (log10 (( double ) (lraise )));
            li += ln + 1;
          }

          /*create a new element ....*/
          pelement = ( bi_list_t * ) malloc(sizeof(bi_list_t));
          /* remember the first element */
          if(pfirst==NULL) pfirst = pelement;
          /* create anchor id nessessary */
          if(panchor==NULL)panchor = ( bi_list_t * ) malloc(sizeof(bi_list_t));
          panchor->pnext=pelement;
          panchor=pelement;
          pelement->dnumber = (double) lstartnumber;
          ( *picount )++;

          /*now all desired elements between start
           and end are added to the list*/          
          for (lj = lstartnumber; lj <= lendnumber - lraise; lj
              += lraise ) {
            /*allocate element*/
            pelement = ( bi_list_t * ) malloc(sizeof(bi_list_t));
            panchor->pnext=pelement;
            panchor=pelement;
            /*create an element with the number
             (startnumber is already in the list!)*/
            pelement->dnumber = (double) (lj + lraise);
            /*and keep in mind that an element was inserted*/
            ( *picount )++;
          }
        }
      }
    }
    /*if no number is found -> go on in the string */
    else
      li++;
  }
  /*debugging level 1: mark begin and end of function */
  if (DEBUGLEVEL > 0) {
    printf("completed function parser\n");
    fflush(stdout);
  }
  /*( *picount )--*/
  printf("parser created %llu entries in list\n", *picount );
  /*return the pointer that points to the start of the bi_list_t */
  return pfirst;
}

/*****************************************************************************
Log-History

$Log: benchit.c,v $
Revision 1.77  2007/07/11 08:03:03  william
changed the ordering of the list to reflect the user input

Revision 1.76  2007/07/10 14:45:52  molka
added compiler version information

Revision 1.75  2007/07/10 09:20:57  william
replaced each tab with 2 whitespaces

Revision 1.74  2007/07/09 14:07:30  william
added bi_list_t datatype and a parserfunction bi_parselist to parse
comma separated intervall-lists. For syntax see the wiki.

Revision 1.73  2007/07/06 05:42:19  rschoene
moved a declaration (declaration have to appear at the beginning of
a structural block)

Revision 1.72  2007/06/28 17:57:20  molka
added is3d to bi_info struct

Revision 1.71  2007/06/28 17:38:38  molka
fixed bug with timer granularity returning too small values

Revision 1.70  2007/06/25 09:18:48  molka
fixed bug with progress files causing a seg fault if localdef variable is missing

Revision 1.69  2007/06/22 17:22:01  rschoene
changed additional information

Revision 1.68  2007/06/22 16:58:51  rschoene
removed progress file it was buuuuugggy!

Revision 1.67  2007/06/21 17:41:24  molka
added localdef variable for directory to store progress information of running measurements

Revision 1.64  2007/06/05 15:12:01  molka
added abort function to allow kernels to stop measurement in case of an error

Revision 1.63  2007/05/22 16:56:18  molka
print progress information to file instead of .... to the console

Revision 1.62  2007/05/03 18:05:18  molka
added warnings for negative problem sizes, not writing them into result files

Revision 1.61  2007/05/03 15:39:26  molka
changed initialisation of xdata.min and xdata.max in write_results() and moved it out of the loop. The previous implementation could leave them uninitialized in case of aborted non-linear measurements because allresults[0] might  be 0.0

Revision 1.60  2007/05/03 12:10:14  molka
fixed bug in write_results that caused benchit not to write any output

Revision 1.59  2007/04/26 15:38:44  molka
added check to write_results() to avoid empty output files

Revision 1.58  2007/03/29 12:16:02  molka
added random number generator

Revision 1.57  2007/03/08 16:09:36  molka
fixed wrong cast in bi_gettimeofday() and bi_gettimeofday_improved()

Revision 1.56  2007/02/16 11:39:57  rschoene
removed an nearly unused variable, which caused an error on romulus

Revision 1.55  2007/02/01 20:11:58  molka
addad PAPI-timer (PAPI_get_real_usecs) and OpenMP-timer (omp_get_wtime)

Revision 1.54  2007/01/26 13:57:27  molka
added signalhandlers for SIGINT and SIGTERM / removed some icc warnings

Revision 1.53  2007/01/19 15:05:11  molka
removed some icc-warnings

Revision 1.52  2007/01/12 15:10:58  molka
modified bi_getenv to first look into Environment

Revision 1.51  2007/01/12 11:17:15  molka
replaced C_compiler_version by c_compiler_version

Revision 1.50  2007/01/12 10:44:35  molka
replaced //-comments and tabs

Revision 1.49  2006/12/18 12:17:39  rschoene
removed empty lines, archShort and archSpeed are unknown, if run as standalone

Revision 1.48  2006/12/07 14:24:24  rschoene
changed behaviour of standalone, added c-info

Revision 1.47  2006/11/23 18:59:15  hackenb
Tabs are evil! Don't use them! Ever!
plus two minor tweaks to supress multiple outputs in MPI runs

Revision 1.46  2006/10/17 14:40:08  hackenb
added seconds to the bit-file-name to prevent short-running kernels
from overwriting their resultfiles

Revision 1.45  2006/10/05 15:11:03  hackenb
added MPI_Abort to safe_exit()

Revision 1.44  2006/10/05 15:02:12  william
test for new Variables + output of these new variables to resultfile

Revision 1.43  2006/09/29 09:41:35  rschoene
lots of comments

Revision 1.42  2006/09/28 14:54:43  rschoene
lot of commenting

Revision 1.41  2006/09/28 13:48:09  william
finished the documentation with doxygen

Revision 1.40  2006/08/09 08:35:15  rschoene
removed debug print

Revision 1.39  2006/08/09 07:07:15  rschoene
added standalone

Revision 1.37  2006/05/11 12:03:25  william
malloc of size zero failed on jump - added a test on zero

Revision 1.36  2006/01/10 16:48:08  hackenb
removed bug (BI_NEG_INFINITY)

Revision 1.35  2006/01/07 23:37:50  william
ups - forgot a ;   - not good  - not good at all

Revision 1.34  2006/01/07 23:19:59  william
BI_INFINITY and BI_NEG_INFINITY do not work - temorary workaround implemented

Revision 1.33  2006/01/04 15:01:12  william
changed INFINITY and NEGINFINITY to BI_INFINITY and BI_NEG_INFINITY because of namingconflicts with other libraries

Revision 1.32  2006/01/04 11:55:10  rschoene
use 2^1000 for inf, if no HUGE_VAL is found

Revision 1.31  2006/01/04 11:25:09  rschoene
added progressbar and check for INFINITY (problems with calculating the ticks)

Revision 1.30  2006/01/03 15:55:30  mickler
- Fix calculation of outmin etc.

Revision 1.29  2005/12/15 02:41:13  mickler
# Converted to new variable names

Revision 1.28  2005/12/13 00:28:26  mickler
# Removed bi_info::log_[xy]axis references
+ Added check for env variable BENCHIT_NUM_CPUS

Revision 1.27  2005/12/12 10:29:14  mickler
# Doxygen documentation added

Revision 1.26  2005/11/29 20:07:36  mickler
# Removed unused variables

Revision 1.25  2005/11/29 10:49:50  mickler
- // comment fixed

Revision 1.24  2005/11/24 17:16:45  mickler
# bi_gettime is THE timer function
# bi_getTime and bi_timer macros for compatibility

!! USE bi_gettime in kernels from now on !!

Revision 1.23  2005/11/24 03:31:05  mickler
- Fixed wrong C++-style // commments

Revision 1.22  2005/11/22 01:32:44  mickler
+ Added BI_GET_CALL_OVERHEAD_FUNC macro
+ Output of selected timer and its precision and overhead
+ Improved getTimerOverhead()
# Removed bi_getTime alias due to name ambiguity with bi_gettime

!! USE bi_gettime() for time measurements !!

Revision 1.21  2005/11/09 14:12:11  mickler
# Code partly beautified (C-Style)
# Old (already commented out) 'get kernelname from kernel name string' removed
# Reworked axis code for easier maintaining
+ New get_axis_properties(), can be used for linear and logarithmic scale
+ Modified .gp output for beautiful standard-plots

Revision 1.20  2005/11/04 16:15:33  mickler
# Changed computation of axis margins

Revision 1.19  2005/10/27 09:32:08  mickler
- Fixed axisticks=0 error

Revision 1.18  2005/10/26 14:13:23  mickler
# sanity check for get_axis_properties

Revision 1.17  2005/10/25 11:56:24  mickler
- Removed deprecated outputfile entries

Revision 1.16  2005/10/24 16:32:16  mickler
- Fixed linear ticks calculation (complete rewrite)

Revision 1.15  2005/08/30 19:27:30  mickler
- FIXED: bug that arose from stripping the comment from the kernelname
  (Output files were saved in the wrong directory)

Revision 1.14  2005/08/30 13:27:26  mickler
- Kernelname without the comment for the database
+ Renamed BENCHIT_KERNEL_NAME_STRING to BENCHIT_KERNELNAME
+ Sanity-check for the comment

Revision 1.13  2005/08/26 11:22:40  mark
The time from 1.1.70 until now - in us - is too large to fit
_precisely_ in a double number. The interesting  time of 1 us becomes
0.95 us due to loss of precision while converting the long number (64bit)
into a double number (51 bin mantisse).
bi_gettimeofday and selectTimer modified with respect to this behavior.

Do ***NOT*** use bi_gettimeofday or bi_gettime directly, use bi_getTime
instead.

Revision 1.12  2005/08/15 15:23:37  mickler
+ Parameters renamed
+ Directorynames in script generation escaped
+ Changed handling of PARAMETERS to old style

Revision 1.11  2005/08/15 09:49:02  mickler
- FIXED: Segmentation fault on run when using libraries

Revision 1.10  2005/08/08 12:40:04  mickler
- Output file string corrected (80 chars limit removed)

Revision 1.9  2005/07/29 11:52:25  wloch
kernel name is no long set in bi getinfo

Revision 1.8  2005/07/21 19:16:25  wloch
debugged parameter file parsing

Revision 1.7  2005/07/21 18:54:23  wloch
implemented parameter file argument

Revision 1.6  2005/07/21 14:22:38  wloch
implemented quiet and parameter parsing

Revision 1.5  2005/07/21 13:32:04  wloch
implemented hashtable dump into bit file

Revision 1.4  2005/07/20 12:42:21  wloch
kernellanguage and libraries are now taken from kernel name

Revision 1.3  2005/07/19 13:28:53  wloch
changed NUMPROZ to NUMPROC

Revision 1.2  2005/07/19 13:23:20  wloch
synchronized bit file output

Revision 1.1.1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

Revision 1.12  2005/07/04 12:10:39  wloch
inserted CVS comments and prepended bi to hashtable function names

Revision 1.11  2005/06/21 12:28:30  wloch
added support for BENCHIT OUTPUT DIR env var

Revision 1.10  2005/06/16 15:09:02  wloch
implemented environment hashing and some command line arguments

Revision 1.9  2005/04/26 09:27:11  wloch
inserted void into bi gettime prototype definition

Revision 1.8  2005/04/04 09:22:03  kluge
removed an other call to uname, BENCHIT_NODE has to be used instead

Revision 1.7  2005/04/04 09:20:11  kluge
removed the call to uname, BENCHIT_NODE has to be used instead

Revision 1.6  2005/03/16 09:48:56  mark
minor fixes

Revision 1.5  2005/02/16 11:57:17  mark
*** empty log message ***

Revision 1.42005/02/16 07:45:48mark
automatic timer selection, bi_getTime() is the best one

Revision 1.32005/02/15 15:58:41mark
getTimerGranularity added

Revision 1.1.1.12004/12/14 21:22:32william
Release 3.0 - created new cvs-tree src2

*****************************************************************************/

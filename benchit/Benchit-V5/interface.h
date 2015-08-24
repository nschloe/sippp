/*******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*/
/*!@file interface.h
 * This interface has to be implemented by all kernels in order to work
 * with the main program. Aditionally
 * - all strings in the info-struct have a variable size. They are
 *   copied char by char into the resultfile (including line-breaks
 *   (don't forget the "\"  *   at the ende of the line))
 * - strings only containing a nullbyte result in "" within the resultfile
 * - nullpointer result in no output
 * - all strings have to be allocated by malloc
 * - all floating point numbers are double
 * - the result-vector contains as first value the x-value then the
 *   y-values, hence the number of doubles in the result-vector should
 *   be numfunctions+1
 *
 * Author: Guido Juckeland (juckeland@zhr.tu-dresden.de)
 * Last Change by: $Author: william $
 * $Revision: 1.18 $
 * $Date: 2007/07/09 14:07:30 $
 */

/******************************************************************************/

#ifndef BENCH_IT_INTERFACE_H
#define BENCH_IT_INTERFACE_H

#ifndef DEBUGLEVEL
#define DEBUGLEVEL 0 /**< set the level of verbosity if not already done */
#endif

/*!@brief Value to mark a measurement as invalid, e.g. due to insufficient timer
 *        resolution.
 */
#define INVALID_MEASUREMENT -7.77E7

/*!@brief Macro for executing the command Y if DEBUGLEVEL is equal to or larger
 * than X.\n Flushes stdout and stderr after the command.
 */
#define IDL(X,Y) if((DEBUGLEVEL)>=(X)){(void)(Y);fflush(stdout);fflush(stderr);}

/*!@brief Template for custom overhead measuring function.
 *
 * This macro is for the use in kernels.\n
 * It constructs the function @c bi_get_call_overhead with arguments specified
 * through X and the function to be called for measuring the overhead must be
 * supplied in Y.\n\n
 * So if you wanted to measure the overhead of your function @c compute(), you
 * would use @c BI_GET_CALL_OVERHEAD_FUNC( (), compute() );\n
 * If your function has arguments which you would like to pass, e.g.
 * compute( double size ), you would it use like that:
 * @c BI_GET_CALL_OVERHEAD_FUNC( (double arg_size), compute(arg_size) );\n
 * The name of the argument is irrelevant, but must be the same in the two
 * definitions.\n\n
 * After you have used this macro you can call the function
 * @c bi_get_call_overhead with your defined arguments.
 * @param X The function arguments for bi_get_call_overhead.
 * @param Y The call to the function, whose overhead shall be measured.
 * @return Time which is needed for one call to the function Y (the so-called 
 *         overhead), in seconds.
 */
#define BI_GET_CALL_OVERHEAD_FUNC(X,Y) \
\
double bi_get_call_overhead X\
{\
  double start, stop, diff;\
  int passes = 1000;\
  int i;\
  do\
  {\
    passes *= 10;\
    start = bi_timer();\
    for( i = 0; i < passes; ++i )\
      (void)Y;\
    stop = bi_timer();\
    diff = stop - start - dTimerOverhead;\
  } while( diff < 10 * dTimerGranularity );\
  return diff / passes;\
}\


/*!@brief Structure for use by kernels to store measuring results and other
 *        information about the kernel.
 */
typedef struct bi_info
{
  /*!@brief Short piece of code showing the function of the kernel. */
  char *codesequence;
  /*!@brief Description of the function of the kernel. */
  char *kerneldescription;
  /*!@brief Description of the x-axis. */
  char *xaxistext;
  /*!@brief Descriptions of the y-axes. */
  char **yaxistexts;
  /*!@brief Legend(s) for the measurement(s). */
  char **legendtexts;
  /*!@brief Actual number of processes the kernel has used. 
   *
   * @li @c -1: The number of processes has changed during the run.
   * @li @c >=1: The kernel has used this many processes.
   */
  int num_processes;
  /*!@brief Actual number of threads per process the kernel has used.
   *
   * @li @c -1: The number of threads per process was not equal for each process
   *            or it has changed during the run.
   * @li @c 0: The kernel does not use threads.
   * @li @c >=1: The kernel has used this many threads per process.
   */
  int num_threads_per_process;
  /*!@deprecated This value was renamed to num_measurements.
   * @brief Maximum problem size to be measured. */
  int maxproblemsize;
  /*!@brief The number of measurements that will be performed. */
  int num_measurements;
  /*!@brief Number of different functions this kernel measures. */
  int numfunctions;
  /*!@brief Boolean value indicating whether outliers are above(1) or below(0) correct results*/
  int *outlier_direction_upwards;
  /*!@brief Boolean value indicating whether this kernel uses MPI version 1. */
  int kernel_execs_mpi1;
  /*!@brief Boolean value indicating whether this kernel uses MPI version 2. */
  int kernel_execs_mpi2;
  /*!@brief Boolean value indicating whether this kernel uses PVM. */
  int kernel_execs_pvm;
  /*!@brief Boolean value indicating whether this kernel uses OpenMP. */
  int kernel_execs_omp;
  /*!@brief Boolean value indicating whether this kernel uses PThreads. */
  int kernel_execs_pthreads;
  /*!@deprecated This value will be removed soon, do not use it in new kernels.
   * @brief Boolean value indicating whether the x-axis uses logarithmic scaling. */
  int log_xaxis;
  /*!@deprecated This value will be removed soon, do not use it in new kernels.
   * @brief Boolean values indicating whether the y-axes use logarithmic scaling. */
  int *log_yaxis;
  /*!@brief Base of the x-axis.
   *
   * @li @c 0: Linear axis.
   * @li @c >1: Logrithmic axis.\n
   *
   * Other values are invalid.
   */
  double base_xaxis;
  /*!@brief Bases of the y-axes.
   *
   * See bi_info::base_xaxis.
   */
  double *base_yaxis;
  /*!@brief Custom options for gnuplot. */
  char *gnuplot_options;
  /*!@brief additional information the kernel can specify
   * this is suposed to be a comma seperated list of key=value settings
   * the string will be put into the result file, some values may be used
   * by the BenchIT-GUI and the Webpage 
   * a list of accepted strings is maintaned in the Wiki
   */
  char *additional_information;
  /*!@brief Boolean values indicating whether the kernel produces 3-dimensional output*/
  int is3d;
} bi_info;


/*!@brief structure for a chained list that can safe values with 
 * the precision of the double-type 
 */
typedef struct bi_list_t{
   /*!@brief value */
   double dnumber;
   /*!@brief pointer to next element of the list */
   struct bi_list_t *pnext;
   }
bi_list_t;

/******************************************************************************/
/*!
 * Functions and variables provided by benchit.c
 */


/*!@brief Duplicates a given string.
 * @param[in] str The string that shall be copied.
 * @return Pointer to the copy of the string.
 */
extern char* bi_strdup(const char* str);

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
extern char* bi_getenv(const char *env, int exitOnNull );

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
extern long int bi_int_getenv(const char *env, int exitOnNull );

/*!@brief Converts $BENCHIT_ARCH_SPEED into float [GHz]
 * @return Clock rate in GHz as float.
 */
extern float bi_cpu_freq(void);

/*!@brief THE function to use in kernels to measure time.
 *
 * Gets the time with the best (=precisest) available timer function.\n\n
 * See also #dTimerGranularity and #dTimerOverhead.
 * @return A time value in seconds.\n The relation of the time depends on the
 *         underlying function but has no relevance for the kernel programmer
 *         since only time differences come to use for measuring elapsed times.
 */
extern double (*bi_gettime)();

/*!@brief Tries to confuse the Cache by filling nCacheSize bytes with
 * data and calculating with it
 * @param[in] nCacheSize number of bytes to allocate
 *            (should be a multiple of sizeof(int))
 * @returns a number which can be ignored ;)
 */
extern int bi_confuseCache(int nCacheSize); 

/*!@deprecated This function alias will be removed soon, do not use it in new
 *             kernels.
 * @brief Solely for compatibility with older kernels!
 *
 * bi_getTime() and bi_timer() are for backwards compatibility only and will be
 * removed soon.\n New kernels MUST NOT use them.
 */
#define bi_getTime() bi_gettime()

/*!@deprecated This function alias will be removed soon, do not use it in new
 *             kernels.
 * @brief Solely for compatibility with older kernels!
 *
 * bi_getTime() and bi_timer() are for backwards compatibility only and will be
 * removed soon.\n New kernels MUST NOT use them.
 */
#define bi_timer() bi_gettime()

/*!@brief Granularity of bi_gettime() in seconds.
 *
 * This variable holds the resolution of bi_gettime() in seconds.\n
 * If you use bi_gettime() calls for time measurement, you
 */
extern double dTimerGranularity;

/*!@brief Call overhead of bi_gettime() in seconds.
 *
 * This variable holds the overhead of bi_gettime() in seconds
 */
extern double dTimerOverhead;

/*!@brief aborts the measurement gracefully.
 *
 * Abort function that should be used by the kernels in case of an error instead of doing an exit(err);
 * writes existing results if any.
 */
extern void bi_abort(int err);


/*! @brief returns a 32-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48() share one state so a call to
 *  bi_random32() will affect the next result of bi_random48() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
extern unsigned int bi_random32(void);


/*! @brief returns a 48-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48()share one state so a call to
 *  bi_random48() will affect the next result of bi_random32() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
extern unsigned long long bi_random48(void);

/*! @brief initalizes random number generator
 *  Initializes tehe random number generator with the values given to the function.
 *  The random number generator uses 2 independent generators and returns the bitwise xor of them.
 *  both generators use this formula: r(n+1) = ((a * r(n)) +b) mod m.
 *  @param[in] start start value for random number generation
 *  @param[in] max the generator will allways return numbers smaller than max
 *                 if max is 0 bi_random32 will return numbers between 0 and 2^32 -1
 *                             bi_random48 will return numbers between 0 and 2^48 -1
 */
extern void bi_random_init(unsigned long long start,unsigned long long max);


/*! @brief The function parses a list of numbers in a certain sysntax 
 * and returns a chained list with the expanded numbers.
 *  @param[out] count holds the number of elements in the result-list
 *  @param[in] pcstring the string containing the intervalls 
 *  @return expanded list of values which is count elements long
 */
bi_list_t *bi_parselist( unsigned long long *picount, const char *pcstring );
/******************************************************************************/
/*!
 * Functions needed by benchit.c. Have to be implemented by the kernels
 */

/*!@brief Provides an empty bi_info struct which has to be filled by the kernel.
 *
 */
extern void bi_getinfo( bi_info* infostruct );

/*!@brief Initialize the kernel.
 *
 * Initializes the kernel with the maximum problem size. The received pointer is
 * given later with each call to bi_entry() in mcb.
 * @param[in] problemsizemax Maximum problem size that will be used for
 *            measuring.
 * @return Pointer to allocated memory.
 */
extern void *bi_init( int problemsizemax );

/*!@brief Start a measurement with the specified problem size.
 *
 * Calls the kernel with one problem size.\n Provides as first argument the
 * pointer that was returned by bi_init().\n Results shall be written to
 * *results.
 * @param mcb The pointer received from bi_init().
 * @param problemsize The problem size which shall be measured in this run.
 * @param results Memory to store the results in.
 * @return 0 if everything went alright.\n
 *         1 if an error occured.
 */
extern int bi_entry( void *mcb, int problemsize, double *results );

/*!@brief This function is called at the end of the measurements.
 *
 * The kernel should at least free the memory which it allocated in bi_init().
 * @param mcb The pointer received from bi_init().
 */
extern void bi_cleanup( void *mcb );

#endif

/******************************************************************************
 *  Log-History
 *
 *  $Log: interface.h,v $
 *  Revision 1.18  2007/07/09 14:07:30  william
 *  added bi_list_t datatype and a parserfunction bi_parselist to parse
 *  comma separated intervall-lists. For syntax see the wiki.
 *
 *  Revision 1.17  2007/06/28 17:57:20  molka
 *  added is3d to bi_info struct
 *
 *  Revision 1.16  2007/06/21 12:30:20  molka
 *  added additional information string to info struct and write results
 *
 *  Revision 1.15  2007/06/05 15:12:01  molka
 *  added abort function to allow kernels to stop measurement in case of an error
 *
 *  Revision 1.14  2007/03/29 12:16:02  molka
 *  added random number generator
 *
 *  Revision 1.13  2007/01/26 13:57:27  molka
 *  added signalhandlers for SIGINT and SIGTERM / removed some icc warnings
 *
 *  Revision 1.12  2006/09/28 13:48:23  william
 *  finished the documentation with doxygen
 *
 *  Revision 1.11  2005/12/14 14:37:46  mickler
 *  + Added bi_info::kerneldescription
 *
 *  Revision 1.10  2005/12/14 14:20:33  mickler
 *  + Added bi_info::num_measurements
 *
 *  Revision 1.9  2005/12/13 21:33:10  mickler
 *  + Added num_processes and num_threads_per_process to bi_info struct
 *
 *  Revision 1.8  2005/12/12 10:29:14  mickler
 *  # Doxygen documentation added
 *
 *  Revision 1.7  2005/11/24 17:16:45  mickler
 *  # bi_gettime is THE timer function
 *  # bi_getTime and bi_timer macros for compatibility
 *
 *  !! USE bi_gettime in kernels from now on !!
 *
 *  Revision 1.6  2005/11/24 03:32:48  mickler
 *  # Removed obsolete external declaration of bi_gettime()
 *  - Fixed C++-style comments
 *
 *  Revision 1.5  2005/11/22 01:32:44  mickler
 *  + Added BI_GET_CALL_OVERHEAD_FUNC macro
 *  + Output of selected timer and its precision and overhead
 *  + Improved getTimerOverhead()
 *  # Removed bi_getTime alias due to name ambiguity with bi_gettime
 *
 *  !! USE bi_timer() for time measurements !!
 *
 *  Revision 1.4  2005/07/29 11:52:25  wloch
 *  kernel name is no long set in bi getinfo
 *
 *  Revision 1.3  2005/07/20 12:42:22  wloch
 *  kernellanguage and libraries are now taken from kernel name
 *
 *  Revision 1.2  2005/07/19 12:16:50  wloch
 *  added cvs footer
 *
 *
 */

/*
 * kernel_wrapper.c
 * UFla, 10/2006
 */

#include <stdio.h>
#include <stdlib.h>

#include "interface.h"
#include "kernel_wrapper.h"

static int NUM_KERNEL_VERSIONS = 5;

static int START     = 0;
static int INCREMENT = 0;
static int STEPS     = 0;

/** ==========================================================================
*** FUNCTION bi_getinfo <<<
*** ======================================================================= */
void bi_getinfo(bi_info *info)
{
        char *p  = NULL;

        /* Get kernel environment */

        p = bi_getenv("BENCHIT_KERNEL_START",0);

        if (p!=0)
        {
                START = atoi(p);
        }
        else
        {
                fprintf(stderr,"bi_getinfo: BENCHIT_KERNEL_START not set.\n");
                exit(1);
        }

        p = bi_getenv("BENCHIT_KERNEL_INCREMENT",0);

        if (p!=0)
        {
                INCREMENT = atoi(p);
        }
        else
        {
                fprintf(stderr,"bi_getinfo: BENCHIT_KERNEL_INCREMENT not set.\n");
                exit(1);
        }

        p = bi_getenv("BENCHIT_KERNEL_STEPS",0);

        if (p!=0)
        {
                STEPS = atoi(p);
        }
        else
        {
                fprintf(stderr,"bi_getinfo: BENCHIT_KERNEL_STEPS not set.\n");
                exit(1);
        }

        /* *** Fill info struct *** */

        (void) memset(info,0,sizeof(bi_info));

        /* Scalar data */
        info->codesequence          = bi_strdup("z = L*u (factorised)");
        info->xaxistext             = bi_strdup("Problem dimension N");
        info->log_xaxis             = 0;
        info->base_xaxis            = 0;
        info->maxproblemsize        = STEPS;
        info->numfunctions          = NUM_KERNEL_VERSIONS;
        info->kernel_execs_mpi1     = 0;
        info->kernel_execs_mpi2     = 0;
        info->kernel_execs_pvm      = 0;
        info->kernel_execs_omp      = 0;
        info->kernel_execs_pthreads = 0;

        /* Y-axis texts */
        info->yaxistexts = malloc((info->numfunctions)*sizeof(char*));

        if (info->yaxistexts==0)
        {
                fprintf(stderr,"bi_getinfo: Allocation of yaxistexts failed.\n");
                exit(1);
        }

        info->yaxistexts[0] = bi_strdup("No. of iterations");
        info->yaxistexts[1] = bi_strdup("No. of iterations");
        info->yaxistexts[2] = bi_strdup("No. of iterations");
        info->yaxistexts[3] = bi_strdup("No. of iterations");
        info->yaxistexts[4] = bi_strdup("No. of iterations");
        
	/* Legend texts */
        info->legendtexts = malloc((info->numfunctions)*sizeof(char*));

        if (info->legendtexts==0)
        {
                fprintf(stderr,"bi_getinfo: Allocation of legendtexts failed.\n");
                exit(1);
        }

        info->legendtexts[0] = bi_strdup("ID");
        info->legendtexts[1] = bi_strdup("BlockJac");
        info->legendtexts[2] = bi_strdup("LowerBLockSOR");
        info->legendtexts[3] = bi_strdup("UpperBLockSOR");
        info->legendtexts[3] = bi_strdup("BlockSSOR");

        /* Base_yaxis */
        info->base_yaxis = malloc((info->numfunctions)*sizeof(int));

        if (info->base_yaxis==0)
        {
                fprintf(stderr,"bi_getinfo: Allocation of base_yaxis failed.\n");
                exit(1);
        }

        info->base_yaxis[0] = 0;
        info->base_yaxis[1] = 0;
        info->base_yaxis[2] = 0;
        info->base_yaxis[3] = 0;
        info->base_yaxis[4] = 0;

        /* Log_yaxis */
        info->log_yaxis = malloc((info->numfunctions)*sizeof(int));

        if (info->log_yaxis==0)
        {
                fprintf(stderr,"bi_getinfo: Allocation of log_yaxis failed.\n");
                exit(1);
        }

        info->log_yaxis[0] = 0;
        info->log_yaxis[1] = 0;
        info->log_yaxis[2] = 0;
        info->log_yaxis[3] = 0;
        info->log_yaxis[4] = 0;

        /* Outlier direction upwards */
        info->outlier_direction_upwards = malloc((info->numfunctions)*sizeof(int));

        if (info->outlier_direction_upwards==0)
        {
                fprintf(stderr,"bi_getinfo: Allocation of outlier_dir_up failed.\n");
                exit(1);
        }

        info->outlier_direction_upwards[0] = 0;
        info->outlier_direction_upwards[1] = 0;
        info->outlier_direction_upwards[2] = 0;
        info->outlier_direction_upwards[3] = 0;
        info->outlier_direction_upwards[4] = 0;

        return;
}

/*
  typedef struct bi_info{
          char *codesequence;
          char *xaxistext;
          char **yaxistexts;
  char **legendtexts;
  int  maxproblemsize;
  int  numfunctions;
  int  *outlier_direction_upwards;
  int  kernel_execs_mpi1;
  int  kernel_execs_mpi2;
  int  kernel_execs_pvm;
  int  kernel_execs_omp;
  int  kernel_execs_pthreads;
  int  log_xaxis;
  int  *log_yaxis;
  double base_xaxis;
  double *base_yaxis;
  char *gnuplot_options;
}
*/

/** ==========================================================================
*** END of FUNCTION bi_getinfo >>>
*** ======================================================================= */

/** ==========================================================================
*** FUNCTION bi_init <<<
*** ======================================================================= */
void *bi_init(int N)
{
  int max_problem_size = START + INCREMENT*(N-1);

  init_(&max_problem_size);

  return NULL;
}
/** ==========================================================================
*** END of FUNCTION bi_init >>>
*** ======================================================================= */

/** ==========================================================================
*** FUNCTION bi_entry <<<
*** ======================================================================= */
int bi_entry(void *data, int N, double *results)
{
        int    problem_size;
        double num_it;
        double T_0,T_1;

        /* Variables for overhead measurement */
        static double T_x = 0.0;   /* The overhead time              */
        int           N_x = 1000;  /* Num of loops to check overhead */
        int           N_0 = 2;     /* Problem size for overhead check */
        int           N_1 = 1;     /* Kernel version for overhead check */
        int           i;

        /* Check and return on invalid pointer */
        if (results==NULL) return 1;

        /* Compute actual problem size (Matrix and vector dimensions) */
        problem_size = START + INCREMENT*(N-1);

        /* Store x axis value in results vector */
        results[0] = (double) problem_size;

        /* Get overhead */
        if (T_x == 0.0)
        {
                for (i=0;i<N_x;i++)
                {
                        T_0 = bi_gettime();
                        kernel_main_(&N_0,&N_1,&num_it);
                        T_1 = bi_gettime();
                        T_x += T_1 - T_0;
                }
                T_x /= (double) N_x;
        }

        /* Call all kernel versions consequtively */
        for (i=1;i<=NUM_KERNEL_VERSIONS;i++)
        {
                /* Call Fortran kernel */
                T_0 = bi_gettime();
                kernel_main_(&problem_size,&i,&num_it);
                T_1 = bi_gettime();

                /* Store y axis value in results vector */
                /* results[i] = num_flop/(T_1-T_0-T_x); */
                results[i] = num_it;
        }

        return 0;
}
/** ==========================================================================
*** END of FUNCTION bi_entry >>>
*** ======================================================================= */

/** ==========================================================================
*** FUNCTION bi_cleanup <<<
*** ======================================================================= */
void bi_cleanup(void *data)
{
        cleanup_();

        return;
}
/** ==========================================================================
*** END of FUNCTION bi_cleanup >>>
*** ======================================================================= */

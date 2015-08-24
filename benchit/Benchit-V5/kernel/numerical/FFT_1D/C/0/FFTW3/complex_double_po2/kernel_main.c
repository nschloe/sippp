/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel:  1D Fast Fourier Transform, Powers of 2,
 *          double precision, complex data, FFTW3 (C)
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: molka $
 * $Revision: 1.4 $
 * $Date: 2007/06/22 07:52:06 $
 *******************************************************************/

/* This file has been automatically generated by the BenchIT kernel generator. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <interface.h>
#include <fft.h>

int n_of_works;
int n_of_sure_funcs_per_work;


void initData_ip(mydata_t* mdpv,int problemsize)
{
   int i;
   for (i = 0; i < problemsize; i++)
   {
      mdpv->inout[i][0] = 0.0;
      mdpv->inout[i][1] = 0.0;
   }
}


void initData_oop(mydata_t* mdpv,int problemsize)
{
   int i;
   for (i = 0; i < problemsize; i++)
   {
      mdpv->in[i][0] = 1.1/(i+1);
      mdpv->in[i][1] = 1.2/(i+1);
      mdpv->out[i][0] = 0.0;
      mdpv->out[i][1] = 0.0;
   }
}


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t* pmydata)
{
   char* p = 0;

   p = bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0);
   pmydata->min = (myinttype)pow(2,(atoi(p)));
   p = bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0);
   pmydata->max = (myinttype)pow(2, (atoi(p)));
   pmydata->steps = (myinttype) log2(pmydata->max - pmydata->min + 2);
}


void bi_getinfo(bi_info* pinfo)
{
   mydata_t* penv;


   (void)memset(pinfo, 0, sizeof(bi_info));
   penv = (mydata_t*)malloc(sizeof(mydata_t));

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup("fftw_execute()");
   pinfo->kerneldescription = bi_strdup("1D Fast Fourier Transform, Powers of 2, double precision, complex data, FFTW3 (C)");
   pinfo->xaxistext = bi_strdup("Problem Size");
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;

   n_of_works = 2; /* in place, out of place */
   n_of_sure_funcs_per_work = 1; /* FLOPS (calculated) */
   pinfo->numfunctions = n_of_works * n_of_sure_funcs_per_work;

   /* allocating memory for y axis texts and properties */
   pinfo->yaxistexts = malloc(pinfo->numfunctions * sizeof(char*));
   if (pinfo->yaxistexts == NULL) {
     fprintf(stderr, "Allocation of yaxistexts failed.\n"); fflush(stderr); exit(127);
   }
   pinfo->outlier_direction_upwards = malloc(pinfo->numfunctions * sizeof(int));
   if (pinfo->outlier_direction_upwards == NULL) {
     fprintf(stderr, "Allocation of outlier direction failed.\n"); fflush(stderr); exit(127);
   }
   pinfo->legendtexts = malloc(pinfo->numfunctions * sizeof(char*));
   if (pinfo->legendtexts == NULL) {
     fprintf(stderr, "Allocation of legendtexts failed.\n"); fflush(stderr); exit(127);
   }
   pinfo->base_yaxis = malloc(pinfo->numfunctions * sizeof(double));
   if ( pinfo->base_yaxis == NULL ) {
     fprintf(stderr, "Allocation of base yaxis failed.\n"); fflush(stderr); exit(127);
   }

   /* setting up y axis texts and properties */
   pinfo->yaxistexts[0] = bi_strdup("FLOPS");
   pinfo->outlier_direction_upwards[0] = 0;
   pinfo->base_yaxis[0] = 0;
   pinfo->legendtexts[0] = bi_strdup("FLOPS in place");
   pinfo->yaxistexts[1] = bi_strdup("FLOPS");
   pinfo->outlier_direction_upwards[1] = 0;
   pinfo->base_yaxis[1] = 0;
   pinfo->legendtexts[1] = bi_strdup("FLOPS out of place");

   /* this kernel needs a logarithmic x-axis */
   pinfo->base_xaxis = 2.0;


   /* free all used space */
   if (penv)free(penv);
}


void* bi_init(int problemsizemax)
{
   mydata_t* pmydata;

   pmydata = (mydata_t*)malloc(sizeof(mydata_t));
   if (pmydata == 0) {
      fprintf(stderr, "Allocation of structure mydata_t failed\n"); fflush(stderr); exit(127);
   }
   else {
      evaluate_environment(pmydata);
      problemsizemax = (int)pmydata->max;
   }
   return (void*)pmydata;
}


int bi_entry(void * mdpv, int iproblemsize, double * dresults)
{
   /* dstart, dend: the start and end time of the measurement */
   /* dtime: the time for a single measurement in seconds */
   double dstart = 0.0, dend = 0.0, dtime = 0.0, dinit = 0.0;
   /* flops stores the calculated FLOPS */
   double flops = 0.0;
   /* ii is used for loop iterations */
   myinttype ii, jj, imyproblemsize, numberOfRuns;
   /* cast void* pointer */
   mydata_t* pmydata = (mydata_t*)mdpv;
   int invalid = 0;

   /* calculate real problemsize */
   imyproblemsize = (int)pow(2, (log2(pmydata->min) + (myinttype)iproblemsize - 1));

   /* store the value for the x axis in results[0] */
   dresults[0] = (double)imyproblemsize;


   /*** in place run ***/

   /* malloc */
   pmydata->inout = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * imyproblemsize);

   /* create FFT plan */
   pmydata->p = fftw_plan_dft_1d(imyproblemsize, pmydata->inout, pmydata->inout, FFTW_FORWARD, FFTW_ESTIMATE);


   /* init stuff */
   initData_ip(pmydata, imyproblemsize);

   numberOfRuns = 1;

   dstart = bi_gettime();
   /* fft calculation */
   fftw_execute(pmydata->p);
   dend = bi_gettime();

   /* calculate the used time*/
   dtime = dend - dstart;
   dtime -= dTimerOverhead;

   /* loop calculation if accuracy is insufficient */
   while (dtime < 100 * dTimerGranularity) {

     numberOfRuns = numberOfRuns * 2;

     dstart = bi_gettime();
     for (jj = 0; jj < numberOfRuns; jj++) {
       /* fft calculation */
       fftw_execute(pmydata->p);
     }
     dend = bi_gettime();

     dtime = dend - dstart;
     dtime -= dTimerOverhead;
   }

   /* check for overflows */
   for (ii = 0; ii < imyproblemsize; ii++) {
     if (isnan(pmydata->inout[ii][0]) || isnan(pmydata->inout[ii][1])) invalid = 1;
     if (isinf(pmydata->inout[ii][0]) || isinf(pmydata->inout[ii][1])) invalid = 1;
   }

   /* if loop was necessary */
   if (numberOfRuns > 1) dtime = dtime / numberOfRuns;

   /* calculate the used FLOPS */
   flops = (double)(5.0 * imyproblemsize * (log2(1.0 * imyproblemsize)) / dtime);

   /* store the FLOPS in results[1] */
   if (invalid == 1) dresults[1] = INVALID_MEASUREMENT;
     else dresults[1] = flops;

   fftw_destroy_plan(pmydata->p);

   /* free data */
   fftw_free(pmydata->inout);


   /*** out of place run ***/

   /* malloc */
   pmydata->in = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * imyproblemsize);
   pmydata->out = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * imyproblemsize);

   /* create FFT plan */
   pmydata->p = fftw_plan_dft_1d(imyproblemsize, pmydata->in, pmydata->out, FFTW_FORWARD, FFTW_ESTIMATE);

   /* init stuff */
   initData_oop(pmydata, imyproblemsize);

   numberOfRuns = 1;

   dstart = bi_gettime();
   /* fft calculation */
   fftw_execute(pmydata->p);
   dend = bi_gettime();

   /* calculate the used time*/
   dtime = dend - dstart;
   dtime -= dTimerOverhead;

   /* loop calculation if accuracy is insufficient */
   while (dtime < 100 * dTimerGranularity) {

     numberOfRuns = numberOfRuns * 2;

     dstart = bi_gettime();
     for (ii = 0; ii < numberOfRuns; ii++) {
        /* fft calculation */
        fftw_execute(pmydata->p);
     }
     dend = bi_gettime();

     /* calculate the used time*/
     dtime = dend - dstart;
     dtime -= dTimerOverhead;
   }

   /* if loop was necessary */
   if (numberOfRuns > 1) dtime = dtime / numberOfRuns;

   /* check for overflows */
   for (ii = 0; ii < imyproblemsize; ii++) {
     if (isnan(pmydata->out[ii][0]) || isnan(pmydata->out[ii][1])) invalid = 1;
     if (isinf(pmydata->out[ii][0]) || isinf(pmydata->out[ii][1])) invalid = 1;
   }

   /* calculate the used FLOPS */
   flops = (double)(5.0 * imyproblemsize * (log2(1.0 * imyproblemsize)) / dtime);

   /* store the FLOPS in results[2] */
   if (invalid == 1) dresults[2] = INVALID_MEASUREMENT;
     else dresults[2] = flops;

   fftw_destroy_plan(pmydata->p);

   /* free data */
   fftw_free(pmydata->in);
   fftw_free(pmydata->out);

   return 0;
}


void bi_cleanup(void* mdpv)
{
   mydata_t* pmydata = (mydata_t*)mdpv;
   if (pmydata) {
      free(pmydata);
   }
   return;
}


/********************************************************************
 * Log-History
 *
 * $Log: kernel_main.c,v $
 * Revision 1.4  2007/06/22 07:52:06  molka
 * newly generated FFT kernels after template change
 *
 *
 *******************************************************************/

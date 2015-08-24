/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel:  2D Fast Fourier Transform, Non-Powers of 2,
 *          double precision, complex data, MKL (C), OpenMP version
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
#include <omp.h>
/*included for version purposes*/
#include <mkl_blas.h>

int n_of_works;
int n_of_sure_funcs_per_work;


void initData_ip(mydata_t* mdpv,int problemsize)
{
   int i;
   for (i = 0; i < problemsize * problemsize; i++)
   {
      mdpv->inout[2*i] = 0.0;
      mdpv->inout[2*i+1] = 0.0;
   }
}


void initData_oop(mydata_t* mdpv,int problemsize)
{
   int i;
   for (i = 0; i < problemsize * problemsize; i++)
   {
      mdpv->in[2*i] = 1.1/(i+1);
      mdpv->in[2*i+1] = 1.2/(i+1);
      mdpv->out[2*i] = 0.0;
      mdpv->out[2*i+1] = 0.0;
   }
}


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t* pmydata)
{
   char* p = 0;
   myinttype nMeasurements;
   int i;

   p = getenv("BENCHIT_KERNEL_PROBLEMSIZES");
   if (p != NULL)  {
      nMeasurements = 1;
      while (p) {
         p = strstr(p, ",");
         if (p) {
            p++;
            nMeasurements++;
         }
      }
      pmydata->problemsizes = (myinttype *)malloc(sizeof(myinttype) * nMeasurements);

      p = getenv("BENCHIT_KERNEL_PROBLEMSIZES");
      pmydata->problemsizes[0] = atof(p);

      for (i = 1; i < nMeasurements; i++) {
         p = strstr(p, ",") + 1;
         pmydata->problemsizes[i] = atof(p);
      }
   }
   else {
      pmydata->problemsizes = NULL;
   }

   pmydata->min = pmydata->problemsizes[0];
   pmydata->max = pmydata->problemsizes[nMeasurements-1];
   pmydata->steps = nMeasurements;
}


void bi_getinfo(bi_info* pinfo)
{
   mydata_t* penv;

   char buf[150];
   char version[100];
   int loop;

   (void)memset(pinfo, 0, sizeof(bi_info));
   penv = (mydata_t*)malloc(sizeof(mydata_t));

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup("DftiComputeForward()");
   pinfo->kerneldescription = bi_strdup("2D Fast Fourier Transform, Non-Powers of 2, double precision, complex data, MKL (C), OpenMP version");
   pinfo->xaxistext = bi_strdup("Problem Size");
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 1;
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

   /*get MKL version*/
   MKLGetVersionString(version,100);
   for (loop=0;loop<100;loop++)
   {
      if (version[loop]=='\0') break;
   }
   if (loop<100) sprintf(buf,"mklversion=\"%s\"",version);
   else sprintf(buf,"mklversion=\"\"");
   pinfo->additional_information=bi_strdup(buf);

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
   long status, len[2];

   /* calculate real problemsize */
   imyproblemsize = (int)(pmydata->problemsizes[iproblemsize - 1]);

   len[0] = imyproblemsize;
   len[1] = imyproblemsize;

   /* store the value for the x axis in results[0] */
   dresults[0] = (double)imyproblemsize;


   /*** in place run ***/

   /* malloc */
   pmydata->inout = (double*)malloc(sizeof(double) * imyproblemsize * imyproblemsize * 2);

   /* create FFT plan */
   status = DftiCreateDescriptor(&pmydata->my_desc_handle, DFTI_DOUBLE, DFTI_COMPLEX, 2, len);
   status = DftiCommitDescriptor(pmydata->my_desc_handle);


   /* init stuff */
   initData_ip(pmydata, imyproblemsize);

   numberOfRuns = 1;

   dstart = bi_gettime();
   /* fft calculation */
   status = DftiComputeForward(pmydata->my_desc_handle, pmydata->inout);
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
       status = DftiComputeForward(pmydata->my_desc_handle, pmydata->inout);
     }
     dend = bi_gettime();

     dtime = dend - dstart;
     dtime -= dTimerOverhead;
   }

   /* check for overflows */
   for (ii = 0; ii < imyproblemsize * imyproblemsize; ii++) {
     if (isnan(pmydata->inout[2 * ii]) || isnan(pmydata->inout[2 * ii + 1])) invalid = 1;
     if (isinf(pmydata->inout[2 * ii]) || isinf(pmydata->inout[2 * ii + 1])) invalid = 1;
   }

   /* if loop was necessary */
   if (numberOfRuns > 1) dtime = dtime / numberOfRuns;

   /* calculate the used FLOPS */
   flops = (double)(5.0 * imyproblemsize * imyproblemsize * (log2(1.0 * imyproblemsize * imyproblemsize)) / dtime);

   /* store the FLOPS in results[1] */
   if (invalid == 1) dresults[1] = INVALID_MEASUREMENT;
     else dresults[1] = flops;

   status = DftiFreeDescriptor(&pmydata->my_desc_handle);

   /* free data */
   free(pmydata->inout);


   /*** out of place run ***/

   /* malloc */
   pmydata->in = (double*)malloc(sizeof(double) * imyproblemsize * imyproblemsize * 2);
   pmydata->out = (double*)malloc(sizeof(double) * imyproblemsize * imyproblemsize * 2);

   /* create FFT plan */
   status = DftiCreateDescriptor(&pmydata->my_desc_handle, DFTI_DOUBLE, DFTI_COMPLEX, 2, len);
   status = DftiSetValue(pmydata->my_desc_handle, DFTI_PLACEMENT, DFTI_NOT_INPLACE);
   status = DftiCommitDescriptor(pmydata->my_desc_handle);

   /* init stuff */
   initData_oop(pmydata, imyproblemsize);

   numberOfRuns = 1;

   dstart = bi_gettime();
   /* fft calculation */
   status = DftiComputeForward(pmydata->my_desc_handle, pmydata->in, pmydata->out);
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
        status = DftiComputeForward(pmydata->my_desc_handle, pmydata->in, pmydata->out);
     }
     dend = bi_gettime();

     /* calculate the used time*/
     dtime = dend - dstart;
     dtime -= dTimerOverhead;
   }

   /* if loop was necessary */
   if (numberOfRuns > 1) dtime = dtime / numberOfRuns;

   /* check for overflows */
   for (ii = 0; ii < imyproblemsize * imyproblemsize; ii++) {
     if (isnan(pmydata->out[2 * ii]) || isnan(pmydata->out[2 * ii + 1])) invalid = 1;
     if (isinf(pmydata->out[2 * ii]) || isinf(pmydata->out[2 * ii + 1])) invalid = 1;
   }

   /* calculate the used FLOPS */
   flops = (double)(5.0 * imyproblemsize * imyproblemsize * (log2(1.0 * imyproblemsize * imyproblemsize)) / dtime);

   /* store the FLOPS in results[2] */
   if (invalid == 1) dresults[2] = INVALID_MEASUREMENT;
     else dresults[2] = flops;

   status = DftiFreeDescriptor(&pmydata->my_desc_handle);

   /* free data */
   free(pmydata->in);
   free(pmydata->out);

   return 0;
}


void bi_cleanup(void* mdpv)
{
   mydata_t* pmydata = (mydata_t*)mdpv;
   if (pmydata) {
   free(pmydata->problemsizes);
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

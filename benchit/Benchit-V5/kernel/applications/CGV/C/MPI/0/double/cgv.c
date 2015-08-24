/*******************************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Heat conduction (CG-Solver, vectorized)
 *         Based on the CGV-Exmple in the Parallel Programming Workshop
 *         http://www.hlrs.de/organization/par/par_prog_ws/
 *         Section [42] "Laplace-Example with MPI and PETSc", and
 *         http://www.hlrs.de/organization/par/par_prog_ws/practical/README.html
 *         CG-Solver, vectorized - CGV.tar.gz
 *         by Dr. Rolf Rabenseifner (HLRS)
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2007/01/22 14:54:19 $
 ******************************************************************************/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 00 <<<<<<<<<<<<<<<<<<<<<<*/

#include <stdio.h>
#include <stdlib.h>
#ifndef serial
#  include <mpi.h>
#else
#  include <time.h>
#endif
#include <math.h>
#include <malloc.h>
#include <string.h>
#include "cgv.h"

#ifdef serial
    clock_t time_start_all, time_end_all;
#else
    double time_start_all=1, time_end_all=0;
#endif

/****************** declarations and global data ***************/

/* size of the physical area this process is about to calculate
 * start_i   : start row
 * end1_i    : end row + 1
 * start_j   : start column
 * end1_j    : end column + 1
 * rowsize   : number of data entries in a row of the local chunk
 *             (end1_j - start_j)
 * colsize   : number of data entries in a column of the local chunk
 *             (end1_i - start_i)
 * chunksize : number of data entries in the local chunk
 *             (rowsize * colsize)
 */
int start_i, end1_i, start_j, end1_j, rowsize, colsize, chunksize;

/* SIDX (Serial InDeX): computes the serial index of a two-dimensional array */
/*   input: i = 0...(colsize - 1) and j = 0...(rowsize - 1) */
/*   output: SIDX in 0..(colsize*rowsize-1) */
#  define SIDX(i, j) (rowsize * (i) + (j))

/* data structures */

/* chunk of a vector */
typedef double* vector_chunk;
vector_chunk x, b;


#ifndef serial
/* halo structure */
struct halo_struct
{
    double* north;
    int     north_rank;
    int     north_size;
    double* east;
    int     east_rank;
    int     east_size;
    double* south;
    int     south_rank;
    int     south_size;
    double* west;
    int     west_rank;
    int     west_size;
};
struct halo_struct halo;

/* send scratch buffer */
double* send_scratch_buff;
#endif

/********************** memory management **********************/

void abrt(void)
{
#ifndef serial
    MPI_Abort(MPI_COMM_WORLD, -1);
#endif
    exit(-1);
}

/* safe_malloc: Same meaning as malloc(); error handling included */
void* safe_malloc(int size)
{
    void *new;

    if (size == 0) return NULL;

    new = (void*)malloc(size);
    if (new == NULL) {
        printf("Could not allocate memory.\n");
        abrt();
    } 
    return(new);
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 00 <<<<<<<<<<<<<<<<<<<<<<*/
/*>>>>>>>>>>>>>>>>>>>>>> begin of task 01 <<<<<<<<<<<<<<<<<<<<<<*/

/********************** vector routines ************************/

/*------------------------------
 * void alloc_vec
 *
 * RETURNS:
 *   the memory address of the allocated vector
 *
 * TASK:
 *   allocate memory (sizeof(doudle) * (physical size)) for a vector 
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 * USED FUNCTIONS:
 *   APPL:
 *     safe_malloc 
 *   C-lib:
 *     memset
 */
vector_chunk alloc_vec()
{
#ifndef WITH_MEMOPS
    int i;
#endif
    vector_chunk newVec = (vector_chunk) safe_malloc(sizeof(double) * chunksize);
#ifdef WITH_MEMOPS
    memset(newVec, 0, sizeof(double) * chunksize);
#else
    for (i = 0; i < chunksize; i++) {
        newVec[i] = 0;
    }
#endif
    return newVec;
}

/*------------------------------
 * void dupl_vec
 *   IN  vector_chunk v1      : vector 1 (to copy from)
 *   OUT vector_chunk v2      : vector 2 (to copy to)
 *
 * TASK:
 *   do a vector chunk duplication: v2 := v1
 *   easy solution: memcpy (see '> man memcpy')
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 * USED FUNCTIONS:
 *   C-lib:
 *     memcpy
 */
void dupl_vec(vector_chunk v1, vector_chunk v2)
{
#ifdef WITH_MEMOPS
    memcpy(v2, v1, sizeof(double) * chunksize);
#else
    int i;
    for (i = 0; i < chunksize; i++) {
        v2[i] = v1[i];
    }
#endif
}


/*-------------------------------------------
 * void add_vec_vec
 *  IN  vector_chunk v1      : vector 1
 *  IN  double       lambda  : factor lambda
 *  IN  vector_chunk v2      : vector 2
 *  OUT vector_chunk v3      : vector 3
 *
 * RETURNS:
 *   --
 *
 * TASK:
 *   calculate:   v3 := v1 + lambda * v2
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 *
 * USED FUNCTIONS:
 *   --
 */
void add_vec_vec(vector_chunk restrict v1, double lambda, vector_chunk restrict v2,
                 vector_chunk restrict /* out */ v3)
{
    int i;

    /* addition of two vectors, second vector is multiplied by a scalar */
    for (i = 0; i < chunksize; i++) {
        v3[i] = v1[i] + lambda * v2[i];
    }

    /* for the MFLOPS calculation */
    flops += chunksize * 2;
}

/*------------------------------
 * double dot_product_vec_vec
 *  IN  vector_chunk v1      : vector 1
 *  IN  vector_chunk v2      : vector 2
 *
 *  RETURNS:
 *    double                 : dot product
 *
 * TASK:
 *   calculate dot product for vector 1 and vector 2
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 *
 * USED FUNCTIONS:
 *   APPL:
 *     --
 *   MPI :
 *     MPI_Allreduce
 */
double dot_product_vec_vec(vector_chunk v1, vector_chunk v2)
{
    int i;
    double sum, global_sum;

    /* sum = dot product of vectors */
    sum = 0;
    for(i = 0; i < chunksize; i++) {
        sum += v1[i] * v2[i];
    }

/* global sum */
#ifdef serial
    global_sum = sum;
#else 
          MPI_Allreduce(&sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, comm_cart);
    flops += 1;
#endif

    /* for the MFLOPS calculation */
    flops += chunksize * 2;

    return global_sum;
}


/*------------------------------
 * double max_norm_vec
 *  IN  vector_chunk v1      : vector 1
 *
 *  RETURNS:
 *    double                 : maximum norm
 *
 * TASK:
 *   calculate (vector maximum norm) for vector 1
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 *
 * USED FUNCTIONS:
 *   APPL:
 *     -- 
 *   MPI :
 *     MPI_Allreduce
 */
double max_norm_vec(vector_chunk v1)
{
    int i;
    double maxabs, global_max;

    /* maxabs = maximum norm of the local vector */
          maxabs = 0;
          for(i = 0; i < chunksize; i++) {
              if ( v1[i] > maxabs) maxabs =  v1[i];
              if (-v1[i] > maxabs) maxabs = -v1[i];
          }

    /* global maximum */
#ifdef serial
          global_max = maxabs;
#else
          MPI_Allreduce(&maxabs, &global_max, 1, MPI_DOUBLE, MPI_MAX, comm_cart);
#endif

    /* no flops added, because we have only assignments and no operations */
          return global_max;
}

/*------------------------------
 * double sqr_norm_vec
 *  IN  vector_chunk v1      : vector 1
 *
 *  RETURNS:
 *    double            : square of norm
 *
 * TASK:
 *   calculate global (vector norm Euklid)^2 for vector 1
 *
 * GLOBAL DATA:
 *   IN int chunksize : size of the physical area this process is to calculate
 *
 * USED FUNCTIONS:
 *   APPL:
 *     -- 
 *   MPI :
 *     MPI_Allreduce
 */
double sqr_norm_vec(vector_chunk v1)
{
    int i;
    double sum, global_sum;

    /* sum = dot product of vector */
          sum = 0;
          for(i = 0; i < chunksize; i++) {
              sum += v1[i] * v1[i];
          }

    /* global sum */
#ifdef serial
          global_sum = sum;
#else
          MPI_Allreduce(&sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, comm_cart);
          flops += 1;
#endif

    /* for the MFLOPS calculation */
    flops += chunksize * 2;

          return global_sum;
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 01 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 02 <<<<<<<<<<<<<<<<<<<<<<*/

/*********************** halo routines ************************/
#ifndef serial
/*------------------------------
 * double* alloc_halo
 *   IN  int size: number of values (doubles) in the halo
 * RETURN:
 *   double* : the adress of the halo data
 * TASK:
 *   Allocates memory for the halo.
 * GLOBAL DATA:
 *   --
 * USED FUNCTIONS:
 *   APPL:
 *     safe_malloc
 *   C-lib:
 *     memset
 */
double* alloc_halo(int size)
{
#ifndef WITH_MEMOPS
    int i;
#endif
    double* halo = (double*) safe_malloc(sizeof(double) * size);
#ifdef WITH_MEMOPS
    memset(halo, 0, sizeof(double) * size);
#else
    for (i = 0; i < size; i++) {
        halo[i] = 0;
    }
#endif
    return halo;
}

/*------------------------------
 * void Init_Halo_Struct
 *
 * TASK:
 *   Initialize the halo structure.
 *
 * GLOBAL DATA:
 *   IN int rowsize : number of data entries in a row of the local chunk
 *   IN int colsize : number of data entries in a column of the local chunk
 *   IN/OUT halo_structure halo : the halo
 *   OUT double* send_scratch_buff: scratch buffer for sending data
 * USED FUNCTIONS:
 *   APPL:
 *     safe_malloc, alloc_halo
 *   MPI:
 *     MPI_Cart_shift
 */
void Init_Halo_Struct()
{
    int maxScratchBuffSize; 

    /* allocate sufficient memory for the send scratch buffer */
    maxScratchBuffSize = (rowsize > colsize)?rowsize:colsize;
    send_scratch_buff = (double*) safe_malloc(sizeof(double) * maxScratchBuffSize);

    /* determine neighbors (1-dim and 2-dims) */
    halo.north = alloc_halo(rowsize);
    halo.north_size = (rowsize);
    halo.south = alloc_halo(rowsize);
    halo.south_size = (rowsize);
    /* compute north_rank and south_rank */
          MPI_Cart_shift(comm_cart, 0, 1, &(halo.north_rank), &(halo.south_rank));

    halo.west = alloc_halo(colsize);
    halo.west_size = (colsize);
    halo.east = alloc_halo(colsize);
    halo.east_size = (colsize);
    /* compute west_rank and east_rank */
          MPI_Cart_shift(comm_cart, 1, 1, &(halo.west_rank), &(halo.east_rank)); 
}

void Free_Halo_Struct()
{
    free(send_scratch_buff);
    free(halo.north);
    free(halo.south);
    free(halo.west);
    free(halo.east);
}


/* halo communication: e.g. FROM_WEST_TO_EAST
 *
 *     *********************    *** *********************
 *     *                *  *    *W* *                   *
 *     *  WEST PROCESS  * -*--->*E* *  EAST PROCESS     *
 *     *                *  *    *S* *                   *
 *     *                *  *    *T* *                   *
 *     *********************    *** *********************
 *                               |
 *                               -----> WEST HALO
 */

/*------------------------------
 * void comm_vec_halo
 *
 * IN  vector_chunk v1: the vector with the data to send to other processes
 *
 * TASK:
 *   Send neccessary data to the neighbors
 *   and receive the needed data from the neighbors into
 *   the halo.
 *
 * USED GLOBAL DATA:
 *   IN OUT halo_structure halo              : the halo
 *   IN OUT double*        send_scratch_buff : scratch send buffer
 *   IN int rowsize : number of data entries in a row of the local chunk
 *   IN int colsize : number of data entries in a column of the local chunk
 *
* USED FUNCTIONS:
 *   MPI:
 *     MPI_Irecv, MPI_Send, MPI_Waitall
 */
/* message tags for the different communication directions */
#define TAG_FROM_NORTH_TO_SOUTH 101
#define TAG_FROM_EAST_TO_WEST   102
#define TAG_FROM_SOUTH_TO_NORTH 103
#define TAG_FROM_WEST_TO_EAST   104
void comm_vec_halo(vector_chunk v1)
{
    int i;
    MPI_Request req_array[4];
    MPI_Status  status_array[4];
    
    /* make a non-blocking receive */

    /* Receive halo (non blocking). If current rank has no neighbors, */
    /* the neighbor rank is MPI_PROC_NULL and nothing is sent. */
    /* receive north halo */
    MPI_Irecv(halo.north, halo.north_size, MPI_DOUBLE, halo.north_rank,
              TAG_FROM_NORTH_TO_SOUTH, comm_cart, &req_array[0]);
    /* receive east halo */
          MPI_Irecv(halo.east, halo.east_size, MPI_DOUBLE, halo.east_rank,
                    TAG_FROM_EAST_TO_WEST, comm_cart, &req_array[1]);
    /* receive south halo */
          MPI_Irecv(halo.south, halo.south_size, MPI_DOUBLE, halo.south_rank,
                    TAG_FROM_SOUTH_TO_NORTH, comm_cart, &req_array[2]);
    /* receive west halo */
          MPI_Irecv(halo.west, halo.west_size, MPI_DOUBLE, halo.west_rank,
                    TAG_FROM_WEST_TO_EAST, comm_cart, &req_array[3]);
    
    /* send to north neighbor (blocking) */
    MPI_Send(&v1[SIDX(0, 0)], halo.north_size, MPI_DOUBLE, halo.north_rank,
             TAG_FROM_SOUTH_TO_NORTH, comm_cart);

    /* do we have a east neighbor? */
          if (halo.east_rank != MPI_PROC_NULL) {
        /* fill scratch send buffer with last column of the local chunk */
              for (i = 0; i < halo.east_size; i++) {
                  send_scratch_buff[i] = v1[SIDX(i, (rowsize - 1))];
              }
          }
    /* send to east neighbor (blocking) */
    MPI_Send(send_scratch_buff, halo.east_size, MPI_DOUBLE, halo.east_rank,
             TAG_FROM_WEST_TO_EAST, comm_cart);

    /* send to south neighbor (blocking) */
          MPI_Send(&v1[SIDX((colsize - 1), 0)], halo.south_size, MPI_DOUBLE, halo.south_rank,
                   TAG_FROM_NORTH_TO_SOUTH, comm_cart);
    
    /* do we have a west neighbor? */
          if (halo.west_rank != MPI_PROC_NULL) {
        /* fill scratch send buffer with first column of the local chunk */
              for (i = 0; i < halo.west_size; i++) {
                  send_scratch_buff[i] = v1[SIDX(i, 0)];
              }
          }
    /* send to west neighbor (blocking) */
          MPI_Send(send_scratch_buff, halo.west_size, MPI_DOUBLE, halo.west_rank,
                   TAG_FROM_EAST_TO_WEST, comm_cart);

    /* wait for all halo info */
          MPI_Waitall(4, req_array, status_array);
}
#endif
/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 02 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 03 <<<<<<<<<<<<<<<<<<<<<<*/

/********************** matrix-vector-multiply ****************************/

/* OVERVIEW:
 * 1) Calculation of the "inner values"
 * 2) Calculation of the "border values" of the first row
 * 3) Calculation of the "border values" of the last row
 * 4) Calculation of the "border values" of the first column
 * 5) Calculation of the "border values" of the last column
 * 6) Calculation of the "corner values"
 */

/*------------------------------
 * mult_A_vec
 *      IN  vector_chunk v1 : vector to multiply A with
 *      OUT vector_chunk v2 : output vector
 *
 * TASK: Calculates matrix vector product v2 = A * v1
 *
 * USED GLOBAL DATA: 
 *   IN int start_i   : start row of the physical area this process is to calculate
 *   IN int end1_i    : end row + 1 of the physical area this process is to calculate
 *   IN int start_j   : start column of the physical area this process is to calculate
 *   IN int end1_j    : end column + 1 of the physical area this process is to calculate
 *   IN int rowsize   : number of data entries in a row of the local chunk
 *   IN int colsize   : number of data entries in a column of the local chunk
 *   IN halo_structure halo : the halo data from the neighbors
 *
 * USED FUNCTIONS:
 *   C-lib:
 *     printf
 */
void mult_A_vec(vector_chunk restrict v1, vector_chunk restrict /* out */ v2) 
{
    int i, j;

    /************************************************************************
     * 1) Calculation of the "inner values"                                 *
     ************************************************************************/

    /* first we calculate the "inner" values that do not access the boundaries */
    /* nor the halo */
    for (i = 1; i < (colsize - 1); i++) {
        for (j = 1; j < (rowsize - 1); j++) {
            v2[SIDX(i, j)] = -1 * v1[SIDX((i - 1), j)] +
                             -1 * v1[SIDX(i, (j - 1))] +    
                              4 * v1[SIDX(i, j)] +
                             -1 * v1[SIDX(i, (j + 1))] +
                             -1 * v1[SIDX((i + 1), j)];
        }
    }
    /* for the MFLOPS calculation */
    flops += (rowsize - 2) * (colsize - 2) * 9;

    /************************************************************************
     * 2) Calculation of the "border values" of the first row               *
     ************************************************************************/

    /* second the values in the first row of the local chunk */
    /* not taking the corner values at (start_i, start_j) and (start_i, end1_j - 1) */ 

    /* there are 2 different cases where we have to calculate the values differently */

    /* case A1: boundaries in the north, values in the south -> first row in the physical problem, */
    /*          local chunk has more than one row (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    if (start_i == 0) {
        for (j = 1; j < (rowsize - 1); j++) {
            /* i is set to 0 and the boundaries are taken into account */
            v2[SIDX(0, j)] = -1 * v1[SIDX(0, (j - 1))] +  
                              4 * v1[SIDX(0, j)] +
                             -1 * v1[SIDX(0, (j + 1))] +
                             -1 * v1[SIDX(1, j)];
        }
        /* for the MFLOPS calculation */
        flops += (rowsize - 2) * 7;
    }
#ifndef serial
    /* case A2: halo in the north, values in the south */
    /*          -> somewhere in the middle of the physical problem, */
    /*          local chunk has more than one row (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    else if (start_i > 0) {
        for (j = 1; j < (rowsize - 1); j++) {
            /* i is set to 0 (start_i - start_i) and the halo is taken into account */
            v2[SIDX(0, j)] = -1 * halo.north[j] +
                             -1 * v1[SIDX(0, (j - 1))] +  
                              4 * v1[SIDX(0, j)] +
                             -1 * v1[SIDX(0, (j + 1))] +
                             -1 * v1[SIDX(1, j)];
        }
        /* for the MFLOPS calculation */
        flops += (rowsize - 2) * 9;
    }
#endif


    /************************************************************************
     * 3) Calculation of the "border values" of the last row                *
     ************************************************************************/

    /* third the values in the last row of the local chunk */
    /* not taking the corner values at (end1_i - 1, start_j) */
    /* and (end1_i - 1, end1_j - 1) */ 

    /* there are 2 different cases where we have to calculate the values differently */

    /* case B1: values in the north, boundary in the south */
    /*          -> last row of the physical problem, */
    /*          local chunk has more than one row (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    if (end1_i == m) {
        for (j = 1; j < (rowsize - 1); j++) {
            /* i is set to (colsize - 1) and the boundary is taken into account */
            v2[SIDX((colsize - 1), j)] = -1 * v1[SIDX(((colsize - 1) - 1), j)] +
                                         -1 * v1[SIDX((colsize - 1), (j - 1))] +  
                                          4 * v1[SIDX((colsize - 1), j)] +
                                         -1 * v1[SIDX((colsize - 1), (j + 1))];
        }
        /* for the MFLOPS calculation */
        flops += (rowsize - 2) * 7;
    }
#ifndef serial
    /* case B2: values in the north, halo in the south */
    /*          -> in the middle of the physical problem, */
    /*          local chunk has more than one row (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    else if (end1_i != m) {
        for (j = 1; j < (rowsize - 1); j++) {
            /* i is set to (colsize - 1) and the halo is taken into account */
            v2[SIDX((colsize - 1), j)] = -1 * v1[SIDX(((colsize - 1) - 1), j)] +
                                         -1 * v1[SIDX((colsize - 1), (j - 1))] +  
                                          4 * v1[SIDX((colsize - 1), j)] +
                                         -1 * v1[SIDX((colsize - 1), (j + 1))] +
                                         -1 * halo.south[j];
        }
        /* for the MFLOPS calculation */
        flops += (rowsize - 2) * 9;
    }
#endif

    /************************************************************************
     * 4) Calculation of the "border values" of the first column            *
     ************************************************************************/

    /* fourth the values in the first column of the local chunk */
    /* not taking the corner values at (start_i, start_j) and (end1_i - 1, start_j) */ 

    /* there are 2 different cases where we have to calculate the values differently */
    
    /* case C1: boundary in the west, values in the east */
    /*          -> first column of the physical problem, */
    /*          local chunk has more than one column (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    if (start_j == 0) {
        for (i = 1; i < (colsize - 1); i++) {
            /* j is set to 0 and the boundary is taken into account */
            v2[SIDX(i, 0)] = -1 * v1[SIDX((i - 1), 0)] +
                              4 * v1[SIDX(i, 0)] +
                             -1 * v1[SIDX(i, 1)] +
                             -1 * v1[SIDX((i + 1), 0)];
        }
        /* for the MFLOPS calculation */
        flops += (colsize - 2) * 7;
    }
#ifndef serial
    /* case C2: halo in the west, values in the east */
    /*          -> in the middle of the physical problem, */
    /*          local chunk has more than one column (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    else if (start_j > 0) {
        for (i = 1; i < (colsize - 1); i++) {
            /* j is set to 0 and the halo is taken into account */
            v2[SIDX(i, 0)] = -1 * v1[SIDX((i - 1), 0)] +
                             -1 * halo.west[i] + 
                              4 * v1[SIDX(i, 0)] +
                             -1 * v1[SIDX(i, 1)] +
                             -1 * v1[SIDX((i + 1), 0)];
        }
        /* for the MFLOPS calculation */
        flops += (colsize - 2) * 9;
    }
#endif

    /************************************************************************
     * 5) Calculation of the "border values" of the last column             *
     ************************************************************************/

    /* fifth the values in the last column of the local chunk */
    /* not taking the corner values at (start_i, end1_j - 1) and (end1_i - 1, end1_j) */ 

    /* there are 2 different cases where we have to calculate the values differently */

    /* case D1: values in the west, boundary in the east */
    /*          -> last column of the physical problem, */
    /*          local chunk has more than one column (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    if (end1_j == n) {
        for (i = 1; i < (colsize - 1); i++) {
            /* j is set to (rowsize - 1) and the boundary is taken into account */
            v2[SIDX(i, (rowsize - 1))] = -1 * v1[SIDX((i - 1), (rowsize - 1))] +
                                         -1 * v1[SIDX(i, ((rowsize - 1) - 1))] + 
                                          4 * v1[SIDX(i, (rowsize - 1))] +
                                         -1 * v1[SIDX((i + 1), (rowsize - 1))];
        }
        /* for the MFLOPS calculation */
        flops += (colsize - 2) * 7;
    }
#ifndef serial
    /* case D2: values in the west, halo in the east */
    /*          local chunk has more than one column (we assure that each chunk has an extent */
    /*          of at least 2x2) */
    else if (end1_j != n) {
        for (i = 1; i < (colsize - 1); i++) {
            /* j is set to (rowsize - 1) and the halo is taken into account */
            v2[SIDX(i, (rowsize - 1))] = -1 * v1[SIDX((i - 1), (rowsize - 1))] +
                                         -1 * v1[SIDX(i, ((rowsize - 1) - 1))] + 
                                          4 * v1[SIDX(i, (rowsize - 1))] +
                                         -1 * halo.east[i] +
                                         -1 * v1[SIDX((i + 1), (rowsize - 1))];
        }
        /* for the MFLOPS calculation */
        flops += (colsize - 2) * 9;
    }
#endif

    /************************************************************************
     * 6) Calculation of the "corner values"                                *
     ************************************************************************/
    
    /* at last the values in the corners: (start_i, start_j), */
    /* (start_i, end1_j - 1), (end1_i - 1, start_j), (end1_i - 1, end1_j - 1) */
   
    /* we have to calculate all corner values, because we assure that each */
    /* chunk has at least an extent of 2x2 */
    /* For each corner value, we have different cases due to halo or boundary */
    /* neighborship */ 

    /* corner value at (start_i, start_j)*/
    /* must always be calculated */
    v2[SIDX(0, 0)] = 4 * v1[SIDX(0, 0)];

#ifndef serial
    /* is the corner value at the north boundary? */
    if (start_i > 0) {
        /* No -> halo in the north! */
        v2[SIDX(0, 0)] += -1 * halo.north[0];
    }
    /* is the corner value at the west boundary? */
    if (start_j > 0) {
        /* No -> halo in the west! */
        v2[SIDX(0, 0)] += -1 * halo.west[0];
    }
#endif
    /* we always have a value in the east, because we assure that each chunk has */
    /* at least 2x2 extent */
    v2[SIDX(0, 0)] += -1 * v1[SIDX(0, 1)];
    /* we always have a value in the south, because we assure that each chunk has */
    /* at least 2x2 extent */
    v2[SIDX(0, 0)] += -1 * v1[SIDX(1, 0)];

    /* corner value at (start_i, end1_j - 1) */
    /* must always be calculated */
    v2[SIDX(0, (rowsize - 1))] = -1 * v1[SIDX(0, ((rowsize - 1) - 1))] +
                                  4 * v1[SIDX(0, (rowsize - 1))];
#ifndef serial
    /* are we at the north boundary? */
    if (start_i != 0) {
        /* No -> halo in the north */
        v2[SIDX(0, (rowsize - 1))] += -1 * halo.north[(rowsize - 1)];
    }
    /* are we at the east boundary? */
    if (end1_j != n) {
        /* No -> halo in the east */
        v2[SIDX(0, (rowsize - 1))] += -1 * halo.east[0];
    }
#endif
    /* we always have a value in the south, because we assure that each chunk has */
    /* at least 2x2 extent */
    v2[SIDX(0, (rowsize - 1))] += -1 * v1[SIDX(1, (rowsize - 1))];

    /* corner value at (end1_i - 1, start_j) */
    /* must always be calculated */
    v2[SIDX((colsize - 1), 0)] = -1 * v1[SIDX(((colsize - 1) - 1), 0)] +
                                  4 * v1[SIDX((colsize - 1), 0)];

#ifndef serial
    /* are we at the west boundary? */
    if (start_j != 0) {
        /* No -> halo in the west */
        v2[SIDX((colsize - 1), 0)] += -1 * halo.west[(colsize - 1)];
    }
    /* are we at the south boundary? */
    if (end1_i != m) {
        /* No -> halo in the south */
        v2[SIDX((colsize - 1), 0)] += -1 * halo.south[0];
    }
#endif
    /* we always have a value in the east, because we assure that each chunk has */
    /* at least 2x2 extent */
    v2[SIDX((colsize - 1), 0)] += -1 * v1[SIDX((colsize - 1), 1)];

    /* corner value at (end1_i - 1, end1_j - 1) */
    /* must always be calculated */
    v2[SIDX((colsize - 1), (rowsize - 1))] = -1 * v1[SIDX((colsize - 1), ((rowsize - 1) - 1))] +
                                             -1 * v1[SIDX(((colsize - 1) - 1), (rowsize - 1))] +
                                              4 * v1[SIDX((colsize - 1), (rowsize - 1))];
#ifndef serial
    /* are we at the east boundary? */
    if (end1_j != n) {
        /* No -> halo in the east! */
        v2[SIDX((colsize - 1), (rowsize - 1))] += -1 * halo.east[(colsize - 1)];
    }
    /* are we at the south boundary? */
    if (end1_i != m) {
        /* No -> halo in the south! */
        v2[SIDX((colsize - 1), (rowsize - 1))] += -1 * halo.south[(rowsize - 1)];
    }
#endif
    /* MFLOPS for the corner values are neglected */
}
/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 03 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 04 <<<<<<<<<<<<<<<<<<<<<<*/

#ifndef serial
/******************* domain decomposition **********************/

/* //   Example: m=5, n=7, m_procs=2, n_procs=2 */
/* // */
/* //       |           j=0     1     2     3  |  4     5     6            | */
/* //   ----+----------------------------------+---------------------------+ */
/* //   i=0 |             0     1     2     3  |  4     5     6            | */
/* //     1 | process 1   7     8     9    10  | 11    12    13  process 2 | */
/* //     2 |            14    15    16    17  | 18    19    20            | */
/* //   ----+----------------------------------+---------------------------+ */
/* //     3 | process 3  21    22    23    24  | 25    26    27  process 4 | */
/* //     4 |            28    29    30    31  | 32    33    34            | */
/* //   ----+------------------------------+-------------------------------+ */
/* // */

/*------------------------------
 * Init_2dim
 *
 * TASK:
 *   Do the two dimensional domain decomposition as depicted above.
 *
 * USED GLOBAL DATA:
 *   OUT int start_i   : start row of the physical area this process is to calculate
 *   OUT int end1_i    : end row + 1 of the physical area this process is to calculate
 *   OUT int start_j   : start column of the physical area this process is to calculate
 *   OUT int end1_j    : end column + 1 of the physical area this process is to calculate
 *   IN  int my_rank   : the rank of the current process
 *   IN  int m_procs   : vertical number of processors
 *   IN  int n_procs   : horizontal number of processors
 *   OUT MPI_Comm comm_cart : cartesian communicator
 *
 * USED FUNCTIONS:
 *   C-lib:
 *     printf
 *   MPI:
 *     MPI_Cart_create, MPI_Comm_rank
 */
void Init_2dim()
{
    int dims[2];
    int periods[2];
    int blocksize_i, blocksize_j;
    int startblock_i, startblock_j;

    if ((print_level >= 1) && (my_rank == 0)) {
        printf("Using 2-dim domain decomposition, m_procs=%d, n_procs=%d, numprocs=%d\n",
                m_procs, n_procs, numprocs);
    }

    /* Init the virtual topology (2-dims) */
    dims[0] = m_procs;
    dims[1] = n_procs;
    periods[0] = FALSE;
    periods[1] = FALSE;
          MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, FALSE, &comm_cart);
          MPI_Comm_rank(comm_cart, &my_rank);

    if ((my_rank == 0) && ((m_procs * n_procs) != numprocs)) {
        printf("Number of processors does not fit! Use %d processors instead. "
               "Aborting...\n", (m_procs * n_procs));
        abrt();
    }

    /* the number of rows for each block (the last block might have
     * less than the first blocks) */
          blocksize_i = ((m - 1) / m_procs) + 1;

    /* the number of columns for each block (the last block might have
     * less than the first blocks) */
          blocksize_j = ((n - 1) / n_procs) + 1;

    /* calculate the start and end row and column */
          startblock_i = my_rank / n_procs;
          startblock_j = my_rank % n_procs;
          start_i = blocksize_i * startblock_i;
          end1_i = start_i + blocksize_i;
          if (end1_i > m) end1_i = m;
          start_j = blocksize_j * startblock_j;
          end1_j = start_j + blocksize_j;
          if (end1_j > n) end1_j = n;
     //printf("Rank[%d]: m = %d n = %d, m_procs = %d n_procs = %d\n", my_rank, m, n, m_procs, n_procs);
     //printf("Rank[%d]: start_i = %d end1_i = %d, start_j = %d end1_j = %d\n", my_rank, start_i, end1_i, start_j, end1_j);
     //fflush(stdout);
}
#endif



/*------------------------------
 * Init_Decomp
 *
 * TASK:
 *   Do the domain decomposition.
 *
 * USED GLOBAL DATA:
 *   IN/OUT int rowsize     : number of data entries in a row of the local chunk
 *   IN/OUT int colsize     : number of data entries in a column of the local chunk
 *   IN/OUT int chunksize   : number of data entries in the local chunk
 *   IN     int decomp_dims : dimension of the domain decomposition 
 *   IN     int my_rank     : the rank of the current process
 *
 * USED FUNCTIONS:
 *   APPL:
 *     abrt
 *   C-lib:
 *     printf
 */
void Init_Decomp()
{
#ifndef serial
    switch (decomp_dims) {
        case 2:
            Init_2dim();
            break;
        default:
            printf("Wrong decomp_dims=%d\n",decomp_dims);
            abrt();
    }
#else 
    start_i = 0;
    end1_i = m;
    start_j = 0;
    end1_j = n;
#endif 
    /* calculate the size of the rows, cols and the whole chunk */
    rowsize   = end1_j - start_j;
    colsize   = end1_i - start_i;
    chunksize = rowsize * colsize;

    if ((rowsize < 2) || (colsize < 2)) {
        printf("Each process must have at least 2 entries in each dimension.\n");
        printf("Decrease amount of processes or increase physical area size.\n");
        printf("Rank[%d]: rowsize = %d -- colsize = %d\n", my_rank, rowsize, colsize);
        printf("Aborting.\n");
        abrt();
    }
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 04 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 05 <<<<<<<<<<<<<<<<<<<<<<*/

/************* boundary and exact solution init ****************/

/*------------------------------
 * void Init_Boundary_Vec
 *  OUT vector_chunk b: the boundary vector 
 *
 * TASK: Store all boundary values in the vector memory
 *       (that is already allocated).
 *       In this example, the boundaries are given by:
 *         u(i,j) = h * (i+1)  with  h = 1.0/(m+1) for i=-1,m and j=-1,n
 *
 * GLOBAL DATA:
 *   IN int start_i : start row of the physical area this process is to calculate
 *   IN int end1_i  : end row + 1 of the physical area this process is to calculate
 *   IN int start_j : start column of the physical area this process is to calculate
 *   IN int end1_j  : end column + 1 of the physical area this process is to calculate
 *
 * USED FUNCTIONS:
 *   APPL:
 *     --
 */
void Init_Boundary_Vec(vector_chunk b)
{
    int    i, j;
    double v, h;

    h = 1.0/(m+1);
    /* boundary vector data is all zero at this point */
    for (i = start_i; i < end1_i; i++) {
        for (j = start_j; j < end1_j; j++) {
            v = 0.0;
            /* next line not neccessary, because boundary value is zero */
            /* if (i==0)   v = v + */ /*u(-1,j):*/ /* h * 0; */    /* = 0 */
            /* next line simplified to add 1.0 to vector, because of the boundary value of 1.0 */
            /* if (i==m-1) v = v + */ /*u(m, j):*/ /* h * (m+1);*/ /* = 1 */
            if (i==m-1) v = 1.0;
            if (j==0)   v = v + /*u(i,-1):*/  h * (i+1);
            if (j==n-1) v = v + /*u(i, n):*/  h * (i+1);
            b[SIDX((i - start_i), (j - start_j))] = v;
        }
    }
}

/*------------------------------
 * void Init_Exact_Solution_Vec
 *   OUT vector_chunk u: the local chunk of the exact solution vector
 *
 * TASK: Store the exact solution for the given boundary in the vector u.
 *       In this example, the inner values are given by:
 *       u(i,j) = h * (i+1)  with  h = 1.0/(m+1) for i=0..m-1 and j=0..n-1
 *
 * GLOBAL DATA:
 *   IN int start_i : start row of the physical area this process is to calculate
 *   IN int end1_i  : end row + 1 of the physical area this process is to calculate
 *   IN int start_j : start column of the physical area this process is to calculate
 *   IN int end1_j  : end column + 1 of the physical area this process is to calculate
 *   IN int m       : vertical size of the physical problem
 *
 * USED FUNCTIONS:
 *   --
 */
void Init_Exact_Solution_vec(vector_chunk u)
{
    int i, j;
    double h;

    h = 1.0 / (m + 1);
    for (i = start_i; i < end1_i; i++) {
        for (j = start_j; j < end1_j; j++) {
            u[SIDX((i - start_i), (j - start_j))] = h * (i + 1);
        }
    }
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 05 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 06 <<<<<<<<<<<<<<<<<<<<<<*/

/******************* print routines ********************************/
void appl_print_vec(char *name, double *v, int mprint, int nprint)
{
    int i,j;
    if (my_rank == 0) {
        printf("%s:       j=",name);
        for (j=0; j<n; j++) {   
            if ((n>nprint ? (nprint-1)*(j+n-2)/(n-1) != (nprint-1)*(j+n-1)/(n-1) : 1)) {
                printf(" %5d",j);
            }
        }
        printf("\n");
        for (i=0; i<m; i++) {
            if ((m>mprint ? (mprint-1)*(i+m-2)/(m-1) != (mprint-1)*(i+m-1)/(m-1) : 1)) {
                printf("%s: i=%3d   ",name,i);
                for (j=0; j<n; j++) {
                    if ((n>nprint ? (nprint-1)*(j+n-2)/(n-1) != (nprint-1)*(j+n-1)/(n-1) : 1)) {
                        printf(" %5.3f",v[n * i +j]);
                    }
                }
                printf("\n");
            }
        }
    }
}

void recv_token(void)
{
#ifndef serial
    int dummy;
    MPI_Status status;
    if (my_rank > 0) {
        MPI_Recv(&dummy, 0, MPI_INT, my_rank-1, MSG_TAG, comm_cart, &status);
    }
#endif
}

void flush_and_send_token(void)
{
#ifndef serial
    MPI_Status status;
    int dummy;
#endif
    fflush(stdout);
#ifndef serial
    MPI_Send(&dummy, 0, MPI_INT, (my_rank+1) % numprocs, MSG_TAG, comm_cart);
    /* Process 0 must wait until the last process (numprocs-1) has finished its output */
    if (my_rank == 0) {
        MPI_Recv(&dummy, 0, MPI_INT, numprocs-1, MSG_TAG, comm_cart, &status);
    }
#endif
}

#ifndef serial
void print_halo()
{
    int i;

    recv_token();

    printf("\nHalo of rank %d:\n", my_rank);
    printf("----------------\n");
    if (halo.north_rank == MPI_PROC_NULL) {
        printf("North: NO halo\n");
    } else {
        printf("North:\n");
        printf("------\n");
        printf("values: %d, neighbor rank: %d\n", halo.north_size, halo.north_rank);
        for (i = 0; i < halo.north_size; i++) {
            printf("halo[%d]: %f\n", i, halo.north[i]);
        }
    }
    if (halo.east_rank == MPI_PROC_NULL) {
        printf("East: NO halo\n");
    } else {
        printf("East:\n");
        printf("------\n");
        printf("values: %d, neighbor rank: %d\n", halo.east_size, halo.east_rank);
        for (i = 0; i < halo.east_size; i++) {
            printf("halo[%d]: %f\n", i, halo.east[i]);
        }
    }
    if (halo.south_rank == MPI_PROC_NULL) {
        printf("South: NO halo\n");
    } else {
        printf("South:\n");
        printf("------\n");
        printf("values: %d, neighbor rank: %d\n", halo.south_size, halo.south_rank);
        for (i = 0; i < halo.south_size; i++) {
            printf("halo[%d]: %f\n", i, halo.south[i]);
        }
    }

    if (halo.west_rank == MPI_PROC_NULL) {
        printf("West: NO halo\n");
    } else {
        printf("West:\n");
        printf("-----\n");
        printf("values: %d, neighbor rank: %d\n", halo.west_size, halo.west_rank);
        for (i = 0; i < halo.west_size; i++) {
            printf("halo[%d]: %f\n", i, halo.west[i]);
        }
    }
    printf("\n");   
    flush_and_send_token();
}

void print_halo_struct()
{
    recv_token();

    printf("\nHalo of rank %d:\n", my_rank);
    printf("----------------\n");
    if (halo.north_rank == MPI_PROC_NULL) {
        printf("North: NO halo\n");
    } else {
        printf("North: values: %d, neighbor rank: %d\n", halo.north_size, halo.north_rank);
    }
    if (halo.east_rank == MPI_PROC_NULL) {
        printf("East: NO halo\n");
    } else {
        printf("East: values: %d, neighbor rank: %d\n", halo.east_size, halo.east_rank);
    }
    if (halo.south_rank == MPI_PROC_NULL) {
        printf("South: NO halo\n");
    } else {
        printf("South: values: %d, neighbor rank: %d\n", halo.south_size, halo.south_rank);
    }
    if (halo.west_rank == MPI_PROC_NULL) {
        printf("West: NO halo\n");
    } else {
        printf("West: values: %d, neighbor rank: %d\n", halo.west_size, halo.west_rank);
    }
    printf("\n");

    flush_and_send_token();
}
#endif


void print_vec2d(char *name, vector_chunk v2, int mprint, int nprint)
{
#ifndef serial
    int     i, j, rank;
    double* whole_v2;
    double*     buff;
    int         extent[4], new_offset;
    MPI_Status  status, status2;

    whole_v2 = (double*) safe_malloc(sizeof(double) * n * m);
    buff     = (double*) safe_malloc(sizeof(double) * n * m);

    if (my_rank > 0) {
        /* send the start_i, end1-i, start_j, end1_j */
        extent[0] = start_i;
        extent[1] = end1_i;
        extent[2] = start_j;
        extent[3] = end1_j;
        MPI_Send(extent, 4, MPI_INT, 0, 666, comm_cart);

        /* send the vector data to rank 0 */
        MPI_Send(v2, chunksize, MPI_DOUBLE, 0, MSG_TAG, comm_cart);
    } else {
        /* copy the rank 0 chunk into the whole physical vector */
        for (i = start_i; i < end1_i; i++) {
            for (j = start_j; j < end1_j; j++) {
                whole_v2[n * i + j] = v2[SIDX((i - start_i), (j - start_j))];
            }
        }

        /* receive the halo data and data extent from all other ranks into the buffer */
        /* and copy the buffer to where it belongs into the whole physical vector */
        for (rank = 1; rank < numprocs; rank++) {
            MPI_Recv(extent, 4, MPI_INT, rank, 666, comm_cart, &status);
            MPI_Recv(buff, ((extent[1] - extent[0]) * (extent[3] - extent[2])), MPI_DOUBLE,
                     rank, MSG_TAG, comm_cart, &status2);

            new_offset = extent[3] - extent[2];

            for (i = extent[0]; i < extent[1]; i++) {
                for (j = extent[2]; j < extent[3]; j++) {
                    whole_v2[n * i + j] = buff[(i - extent[0]) * new_offset + (j - extent[2])];
                }
            }
        }

//was        free(buff);
    }

    free(buff); //new

    appl_print_vec(name, whole_v2, mprint, nprint);
    free(whole_v2);
#else
    appl_print_vec(name, v2, mprint, nprint); 
#endif
}

void print_vec(char *name, vector_chunk v1)
{
    int i,j;
    recv_token();
    for (i = start_i; i < end1_i; i++) 
      for (j = start_j; j < end1_j; j++) 
        printf("%s[%2d,%2d] = %6.3f\n", name, i,j, v1[SIDX((i - start_i), (j - start_j))]);
    flush_and_send_token();
}

void print_decomp()
{
    recv_token();
    printf("Rank %d: start_i, end1_i, start_j, end1_j: %d, %d, %d, %d\n",
           my_rank, start_i, end1_i, start_j, end1_j);
    flush_and_send_token();
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 06 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 07 <<<<<<<<<<<<<<<<<<<<<<*/

/************************ CG solver ****************************/

/*------------------------------
 * void CG_solve
 *   OUT int *num_iter  : return the number of iterations
 *   OUT double *norm_r : return the norm of r
 *
 * TASK:
 *   solve Laplace equation
 *
 * GLOBAL DATA:
 *   IN     int          iter_max : maximal number of iterations
 *   IN     double       epsilon  : epsilon for maximal error
 *   IN     vector_chunk b        : local chunk of vector b
 *   IN/OUT vector_chunk x        : local chunk of vector x
 *   IN     int          my_rank  : rank of the local process
 * USED FUNCTIONS:
 *   APPL:
 *     alloc_vec, dupl_vec, comm_vec_halo, print_vec, add_vec_vec
 *     sqr_norm_vec, print_halo, mult_A_vec, dot_product_vec_vec
 *   C-lib:
 *     printf, fflush, sqrt
 */
void CG_solve(int *num_iter, double *norm_r)
{
    /* only p has a halo */
    vector_chunk p, r, v;
    double alpha, alpha_new, lambda;
    int iter;

    /* allocate vectors p, r, v */ 
    p = alloc_vec(); 
    v = alloc_vec();
    r = alloc_vec();

    /* x is initialized with 0 */

    /* instead of p = Ax */
    /* p := x */
    dupl_vec(x, p);

#ifndef serial
    /* update halo */
    comm_vec_halo(p);
#endif

    /* v = Ap */
    mult_A_vec(p, v);
    if (print_level>=4) {
        print_vec("v", v);
    }

    /* p = b - v */
    add_vec_vec(b, -1, v, p);
    if (print_level>=4) {
        print_vec("p", p);
    }

    /* r := p */
    dupl_vec(p, r);
    if (print_level>=4) {
        print_vec("r", r);
    }

    /* alpha = ||r||**2,   Caution: sqr_norm_vec() computes already ||.||**2 */
    alpha = sqr_norm_vec(r);


    for(iter=0; (iter < iter_max) && (alpha > epsilon) ; iter++) {
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            printf("CG - my_rank =%3d, i=%d, alpha=%9.6f\n",my_rank,iter,alpha);
            fflush(stdout);
        }

#ifndef serial
        /* update halo */
        comm_vec_halo(p);
#endif

        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            print_vec("p", p);
#ifndef serial
            print_halo();
#endif
        }

        /* v = Ap */
        mult_A_vec(p, v);
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            print_vec("v", v); 
        }

        /* lambda = alpha / (v, p)2 */
        lambda = alpha / dot_product_vec_vec(v, p);
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            printf("CG - my_rank =%3d, i=%d, vp=%9.6f, lambda=%9.6f\n",
                    my_rank,iter, dot_product_vec_vec(v, p), lambda);
            fflush(stdout);
        }

        /* x := x + lambda * p */
        add_vec_vec(x, lambda, p, x);
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            print_vec("x", x);
        }

        /* r := r - lambda * v */
        add_vec_vec(r, -lambda, v, r);
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            print_vec("r", r);
        }

        /* alpha_new = ||r||**2,   Caution: sqr_norm_vec() computes already ||.||**2 */
        alpha_new = sqr_norm_vec(r);

        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            printf("CG - my_rank =%3d, i=%d, alpha_new=%9.6f\n",
                    my_rank,iter, alpha_new);
            fflush(stdout);
        }

        /* p := r + (alpha_new/alpha) * p */
        add_vec_vec(r, alpha_new/alpha, p, p);
        if ((print_level>=5) || ( (print_level>=4)&&(iter==0) )) {
            print_vec("p", p); 
        }

        /* alpha := alpha_new */
        alpha = alpha_new;

        if ((print_level>=2) && (my_rank==0)) {
            printf("CG i=%d, ||r||**2 = %8.2e %s %8.2e,   ||r|| = %8.2e %s %8.2e\n",iter,
                    alpha_new,(alpha_new<=epsilon ? "<=" : "> "),epsilon,
                    sqrt(alpha_new),(alpha_new<=epsilon ? "<=" : "> "),sqrt(epsilon));
        }
    }

    /* update output-arguments */ 
    *num_iter = iter;
    *norm_r = sqrt(alpha);

    /* free vectors p, r, v */
    free(p);
    free(r);
    free(v);
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 07 <<<<<<<<<<<<<<<<<<<<<<*/

/*>>>>>>>>>>>>>>>>>>>>>> begin of task 08 <<<<<<<<<<<<<<<<<<<<<<*/

/************************ main program *************************/

double cgv_all(int bi_m, int bi_n, int bi_iter_max, int bi_epsilon, int bi_print_level)
{
    int num_iter;
    double norm_r, norm_err, maxnorm_err;
    vector_chunk u, e;
    double time_start=0, time_end=0;

    double time_elapsed_all = 1, mflops = 0, global_mflops = 0; 

#ifndef serial
    {
    time_start_all = MPI_Wtime();
    }
#endif

    flops = 0;

    m = 4;
    n = 4;
    iter_max = 500;
    epsilon = 1e-6;
    print_level = 1;
    decomp_dims = 2;
    mprt = 11; nprt = 11;
    m_procs=2; n_procs=2;
#ifndef serial
    {
        int dims[2]; dims[0] = 0; dims[1] = 0;
        MPI_Dims_create(numprocs, 2, dims);
        m_procs = dims[0]; n_procs = dims[1];
    }
#endif

    if ( my_rank == 0 ) {
      /* Get BenchIT options */
      m = bi_m;
      n = bi_n;
      iter_max = bi_iter_max;
      epsilon = bi_epsilon;
      print_level = bi_print_level;
    }

#ifndef serial
    MPI_Bcast(&m,          1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&n,          1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&iter_max,   1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&epsilon,    1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    MPI_Bcast(&print_level,1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&decomp_dims,1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&m_procs,    1, MPI_INT,    0, MPI_COMM_WORLD);
    MPI_Bcast(&n_procs,    1, MPI_INT,    0, MPI_COMM_WORLD);
#endif

    /* Init the domain decomposition */
    Init_Decomp();
    if (print_level >= 1) {
        print_decomp();
    }

#ifndef serial
    /* !!! determine communication partners for vector x !!! */
    Init_Halo_Struct();
    if (print_level>=4) {
        print_halo_struct();
    }
#endif

    /* reserve chunk of vector b and !!! assign border data */
    b = alloc_vec();
    Init_Boundary_Vec(b);
    if (print_level>=4) {
        print_vec("b",b);
    }

    /* x is initialized with zeros */
    x = alloc_vec();

#ifndef serial
    time_start = MPI_Wtime();
#endif

    CG_solve(&num_iter, &norm_r);

#ifndef serial
    time_end = MPI_Wtime();
#endif

    u = alloc_vec();
    e = alloc_vec();
    Init_Exact_Solution_vec(u);
    /* p = u - x */
    add_vec_vec(u, -1, x, e);
    norm_err = sqrt(sqr_norm_vec(e));
    maxnorm_err = max_norm_vec(e);

    if (print_level>=3) {
        print_vec2d("x",x,m,n);
    }

#ifndef serial
    time_end_all = MPI_Wtime();
    time_elapsed_all = time_end_all - time_start_all;
    mflops = flops / (1000000 * time_elapsed_all);
    MPI_Reduce(&mflops, &global_mflops, 1, MPI_DOUBLE, MPI_SUM, 0, comm_cart);
#else
    time_end_all = clock();
    time_elapsed_all = ((double) (time_end_all - time_start_all)) / CLOCKS_PER_SEC;
    mflops = flops / (1000000 * time_elapsed_all);
    global_mflops = mflops;
#endif
/*
    if (my_rank==0) {
        printf("main: %3d iterations, sqrt(eps) = %8.2e,  normalized: %8.2e\n",num_iter,
                sqrt(epsilon), sqrt(epsilon/(n*m)));
        printf("main:                 ||r||     = %8.2e,  normalized: %8.2e\n",
                norm_r, norm_r/sqrt(n*m));
        printf("main:                 ||error|| = %8.2e,  normalized: %8.2e\n",
                norm_err, norm_err/sqrt(n*m));
        printf("main:                 max(error)= %8.2e = normalized: %8.2e\n",
                maxnorm_err, maxnorm_err);
        printf("main:                 n*m = %d*%d = %d\n",n, m, n*m);
        printf("main:                 %8.2e sec\n",time_elapsed_all);

        printf("Calculated MFLOPS (avg):  %10.6f\n", global_mflops / numprocs);
        printf("Calculated MFLOPS (all):  %10.6f\n", global_mflops);
        printf("%d\t%g\n", n, global_mflops*1000000.0);
    }
*/
    if (print_level>=1) {
        print_vec2d("x",x,mprt,nprt);
    }


#ifndef serial
    Free_Halo_Struct();
    MPI_Comm_free(&comm_cart);
#endif

    free(e);
    free(u);
    free(x);
    free(b);

    return global_mflops*1000000;
}

/*>>>>>>>>>>>>>>>>>>>>>> -end- of task 08 <<<<<<<<<<<<<<<<<<<<<<*/


/********************************************************************
 * Log-History
 *
 * $Log: cgv.c,v $
 * Revision 1.1  2007/01/22 14:54:19  hackenb
 * + initial commit :-)
 *
 *
 *
 *******************************************************************/

/*prototypes for matmul1.c are in interface.h*/
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#include <mpi.h>

#include "interface.h"
#include "fmc_h_machdefs.h"

#if ( defined ( _CRAY )    || \
      defined ( _SR8000 )  || \
      defined ( _USE_OLD_STYLE_CRAY_TYPE ) )
#define tstcas_ TSTCAS 
#endif

/* matmul1.h/definitions
 * SYNOPSIS
 * The variables that are described in the readme and the debuglevel.
 * SEE ALSO
 * kernel/matmul1_double
 ***/
#define MINTIME 1.0e-22

/*defining debuglevel*/
#ifndef DEBUGLEVEL
#define DEBUGLEVEL 0
#endif

/****is* matmul1.h/interface::float_data_struct
 * SYNOPSIS
 * typedef struct float_data_struct
 *    {
 *    double *pda, *pdb, *pdc;
 *    } fds
 * DESCRIPTION
 * structure saves both vectors that are added
 * result is saved in pdb
 ***/

typedef struct float_data_struct
   {
   double *pda, *pdb, *pdc;
   int MATMUL_START, MATMUL_INCREMENT, MATMUL_STEPS, MATMUL_PRECISION;
   } fds;

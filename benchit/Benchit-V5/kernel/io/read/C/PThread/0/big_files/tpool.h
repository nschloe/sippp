/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the header-file for the PThread-Pool and nothing else>
*
*  Author: Thomas William (<william@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2007/04/26 02:33:04 $
*  $State: Exp $
*
******************************************************************************/
#ifndef __iotpool_h
#define __iotpool_h
#endif

#include <pthread.h>

/* this is an standard-interface for threadpools */


/* represents a single request in the requestqueue */
typedef struct tpool_work {
	void               (*routine)();
	void                *arg;
	struct tpool_work   *next;
} tpool_work_t;


/* characteristics and state of a mutexed / synchronized / single thread-pool */	
typedef struct tpool {
	/* pool characteristics */
	int                 num_threads;
        int                 max_queue_size;
        int                 do_not_block_when_full;
        /* pool state */
	pthread_t           *threads;
        int                 cur_queue_size;
	tpool_work_t        *queue_head;
	tpool_work_t        *queue_tail;
	int                 queue_closed;
        int                 shutdown;
	/* pool synchronization */
        pthread_mutex_t     queue_lock;
        pthread_cond_t      queue_not_empty;
        pthread_cond_t      queue_not_full;
	pthread_cond_t      queue_empty;
} *tpool_t;


void tpool_init(
           tpool_t          *tpoolp,
           int              num_threads, 
           int              max_queue_size,
           int              do_not_block_when_full);


int tpool_add_work(
           tpool_t          tpool,
           void             (*routine)(),
	   void             *arg);


int tpool_destroy(
           tpool_t          tpool,
           int              finish);

/***********************************************************
* This is the complete log for this file:
*
* $Log: tpool.h,v $
* Revision 1.1  2007/04/26 02:33:04  william
* fixed errors due to old layout of Kernel (no more RUN.SH)
*
* Revision 1.2  2006/04/27 09:30:19  william
* some minor bugs
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/


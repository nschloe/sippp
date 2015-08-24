/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <This file encapsulates everything needed to run a PThread-Pool>
*
*  Author: Thomas William (<william@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2006/12/14 12:54:15 $
*  $State: Exp $
*
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>

#include <pthread.h>
#include "tpool.h"

/* a global object, to be handed over to main routine . . . */
void *tpool_thread(void *);

void tpool_init(tpool_t   *tpoolp,
		int       num_worker_threads, 
		int       max_queue_size,
                /* the next one is (maybe) needed for non-time consuming 
		   peek-performance reading tests */
		int       do_not_block_when_full)
{
  int i, rtn;
  tpool_t tpool;
   
  /* allocate a pool data structure */ 
  if ((tpool = (tpool_t )malloc(sizeof(struct tpool))) == NULL)
    perror("malloc - pool data structure"), exit(1);

  /* initialisation of the fields */
  tpool->num_threads = num_worker_threads;
  tpool->max_queue_size = max_queue_size;
  tpool->do_not_block_when_full = do_not_block_when_full;

  /*allocate the workerthreads*/
  if ((tpool->threads = 
       (pthread_t *)malloc((int)sizeof(pthread_t)*(int)num_worker_threads)) == NULL)
    perror("malloc - workerthreads"), exit(1);
    
  tpool->cur_queue_size = 0;
  tpool->queue_head = NULL; 
  tpool->queue_tail = NULL;
  tpool->queue_closed = 0;  
  tpool->shutdown = 0;
   
  if ((rtn = pthread_mutex_init(&(tpool->queue_lock), NULL)) != 0)
    fprintf(stderr,"pthread_mutex_init %s",strerror(rtn)), exit(1);
    
  if ((rtn = pthread_cond_init(&(tpool->queue_not_empty), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s",strerror(rtn)), exit(1);
    
  if ((rtn = pthread_cond_init(&(tpool->queue_not_full), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s",strerror(rtn)), exit(1);
    
  if ((rtn = pthread_cond_init(&(tpool->queue_empty), NULL)) != 0)
    fprintf(stderr,"pthread_cond_init %s",strerror(rtn)), exit(1);

  /*create the workerthreads*/
  for (i = 0; i != num_worker_threads; i++) {
    if ((rtn = pthread_create( &(tpool->threads[i]),
			      NULL,
			      tpool_thread,
			      (void *)tpool)) != 0)
      fprintf(stderr,"pthread_create %d",rtn), exit(1);
  }

  *tpoolp = tpool;
}


int tpool_add_work(
		   tpool_t          tpool,
		   void             (*routine) (),
		   void             *arg)
{
  int rtn;
  tpool_work_t *workp;

  if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_lock %d",rtn), exit(1);

  /* no space in queue and this caller doesn't want to wait */
  /* tries to unlock when the queue is full an do_not_block is set*/
  if ((tpool->cur_queue_size == tpool->max_queue_size) &&
      tpool->do_not_block_when_full) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);

    return -1;
  }

  while( (tpool->cur_queue_size == tpool->max_queue_size) &&
	(!(tpool->shutdown || tpool->queue_closed))  ) {

    if ((rtn = pthread_cond_wait(&(tpool->queue_not_full),
				 &(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_cond_wait %d",rtn), exit(1);

  }

  /* the pool is in the process of being destroyed */
  if (tpool->shutdown || tpool->queue_closed) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);
 
    return -1;
  }


  /* allocate work structure */
  if ((workp = (tpool_work_t *)malloc(sizeof(tpool_work_t))) == NULL)
    perror("malloc"), exit(1);
  workp->routine = routine;
  workp->arg = arg;
  workp->next = NULL;
  if (tpool->cur_queue_size == 0) {
    tpool->queue_tail = tpool->queue_head = workp;
    if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_empty))) != 0)
      fprintf(stderr,"pthread_cond_signal %d",rtn), exit(1);;
  } else {
    tpool->queue_tail->next = workp;
    tpool->queue_tail = workp;
  }

  tpool->cur_queue_size++; 
  if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);
  return 1;
}

int tpool_destroy(tpool_t          tpool,
		  int              finish)
{
  int          i,rtn;
  tpool_work_t *cur_nodep;
  

  if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_lock %d",rtn), exit(1);

  /* are we in a shutdown-sequence? */
  if (tpool->queue_closed || tpool->shutdown) {
    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);
    return 0;
  }

  tpool->queue_closed = 1;

  /* If the finish flag is set, wait for workers to 
     drain queue and start shutdown*/ 
  if (finish == 1) {
    while (tpool->cur_queue_size != 0) {
      if ((rtn = pthread_cond_wait(&(tpool->queue_empty),
				   &(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_cond_wait %d",rtn), exit(1);
    }
  }

  tpool->shutdown = 1;

  if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
    fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);


  /* wake up (possibly) sleeping workers, for so they see the shutdown-flag*/
  if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_empty))) != 0)
    fprintf(stderr,"pthread_cond_broadcast %d",rtn), exit(1);
  if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_full))) != 0)
    fprintf(stderr,"pthread_cond_broadcast %d",rtn), exit(1);


  /* now let the workers exit properly */
  for(i=0; i < tpool->num_threads; i++) {
    if ((rtn = pthread_join(tpool->threads[i],NULL)) != 0)
      fprintf(stderr,"pthread_join %d",rtn), exit(1);
  }

  /* now let the workers exit properly*/
  free(tpool->threads);
  while(tpool->queue_head != NULL) {
    cur_nodep = tpool->queue_head->next; 
    tpool->queue_head = tpool->queue_head->next;
    free(cur_nodep);
  }
  free(tpool);   
 return 0;
}


/*this is the "Logic" for each workerthread*/
void *tpool_thread(void *arg)
{
  tpool_t tpool = (tpool_t)arg; 
  int rtn;
  tpool_work_t	*my_workp;
	
  for(;;) {

    /* Check queue for work */ 
    if ((rtn = pthread_mutex_lock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_lock %d",rtn), exit(1);

    while ((tpool->cur_queue_size == 0) && (!tpool->shutdown)) {
      
      if ((rtn = pthread_cond_wait(&(tpool->queue_not_empty),
				   &(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_cond_wait %d",rtn), exit(1);

    }

    /* Has a shutdown started while i was sleeping? */
    if (tpool->shutdown == 1) {

      if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
	fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);
      /* if there was a shutdown => exit */
      pthread_exit(NULL);
    }


    /* Get to work, dequeue the next item */ 
    my_workp = tpool->queue_head;
    tpool->cur_queue_size--;
    
    /* in case we took the last-waiting queue-member*/
    if (tpool->cur_queue_size == 0)
      tpool->queue_head = tpool->queue_tail = NULL;
    /* if it was not the last - take the next one*/
    else
      tpool->queue_head = my_workp->next;

    /* Handle waiting add_work threads */
    /* in case, the queue was full - send signal "not full any longer"*/
    if ((!tpool->do_not_block_when_full) &&
	(tpool->cur_queue_size ==  (tpool->max_queue_size - 1))) 
      if ((rtn = pthread_cond_broadcast(&(tpool->queue_not_full))) != 0)
	fprintf(stderr,"pthread_cond_broadcast %d",rtn), exit(1);

    /* Handle waiting destroyer threads */
    if (tpool->cur_queue_size == 0)
      if ((rtn = pthread_cond_signal(&(tpool->queue_empty))) != 0)
	fprintf(stderr,"pthread_cond_signal %d",rtn), exit(1);

    if ((rtn = pthread_mutex_unlock(&(tpool->queue_lock))) != 0)
      fprintf(stderr,"pthread_mutex_unlock %d",rtn), exit(1);
      
   /* now we do the real work :] */
    (*(my_workp->routine))(my_workp->arg);
    free(my_workp);
  } 
  return(NULL); /* may result in a warning - this is OK */           
}

/***********************************************************
* This is the complete log for this file:
*
* $Log: tpool.c,v $
* Revision 1.1  2006/12/14 12:54:15  william
* changed the algorithm a bit for mor informative resultfilesmade chenges to reflect new format of COMPILE.SH and a like
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/


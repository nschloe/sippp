/*********************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
* bubblesort_c/parser.h
* DESCRIPTION
*  header-file of the parser for the bubblesort kernel
*
*  Author:      Sebastian Koelling (koelling@zhr.tu-dresden.de)
*
*  $Revision: 1.2 $
*  $Author: william $
*  $Date: 2006/09/29 16:34:03 $
*  $State: Exp $
*
*********************************************************************/
#ifndef __parse_h
#define __parse_h



#ifndef __int_type_def
#define __int_type_def
#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif
#endif
/****is* parser.h/list
 * SYNOPSIS
 * typedef struct list
 * {
 * long lnumber;
 *  struct list *pnext;
 * }
 * list;
 * DESCRIPTION
 * structure for a chained list that can safe integer values
 ***/
typedef struct list
   {
   /*value */
   myinttype lnumber;
   /*pointer to next element of the list */
   struct list *pnext;
   }
list;

/*prototypes for parser.c*/
list *parse( int *count, const char *string );

#endif

/*********************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
* bubblesort_c/parser.c
* DESCRIPTION
*  parser for the bubblesort kernel
*
*  Author:      Sebastian Koelling (koelling@zhr.tu-dresden.de)
*
*  $Revision: 1.2 $
*  $Author: william $
*  $Date: 2006/09/29 16:34:03 $
*  $State: Exp $
*
****
*********************************************************************/

#include "simple.h"

/* parser.c/functions
 * SYNOPSIS
 * These functions are used by the parser in the bubblesort kernel.
 ***/

/* parser.c/functions::parse
 * SYNOPSIS
 * list *parse( int *picount, const char *pcstring )
 * DESCRIPTION
 * The function parses a list of numbers defined by "pcstring" and
 * returns a chained list with the corresponding numbers.
 * On information how lists should be formated please take a look
 * into the README file.
 * SEE ALSO
 * kernel/bubblesort_c
 ***/
list *parse( int *picount, const char *pcstring )
   {
   /*pointer to the 1st element of the list and 
     return value if the function*/
   list *pfirst;
   /*variable for buffering and working*/
   list *pelement;
   /*loop variables, variables for memorising
      series of numbers and raise*/
   long li, lj, ln, lstartnumber, lendnumber, lraise;

   /*debugging level 1: mark begin and end of function*/
   if ( DEBUGLEVEL > 0)
      {
      printf( "reached function parser\n" );
      fflush(stdout);
      }

   /*initializing*/
   li = 0;
   *picount = 0;
   pfirst = NULL;

   /*as long as the strings end is not reached do ...*/
   while ( pcstring[li] != 0 )
      {
      /*if the beginning of a number is found ...*/
      if ( isdigit( pcstring[li] ) )
         {
         /*create a new element ....*/
         pelement = ( list * ) malloc( sizeof ( list ) );
         /*save the number that was found*/
         sscanf( ( pcstring + li ), "%ld",
                 &pelement->lnumber );
         /*move ahead in the string until the
           end of the number ...*/
         ln = ( long ) ( log10 ( ( double ) ( pelement->lnumber ) ) );
         li += ln + 1;
         /*insert the element (including the number)
           on the beginning of the chained list ...*/
         pelement->pnext = pfirst;
         pfirst = pelement;
         /*and keep in mind that an element was found
           -> count is number of element that have been added
              to the list so far*/
         ( *picount )++;

         /*whitespaces are ignored*/
         if ( isspace( pcstring[li] ) )
            {
            li++;
            }
         /*if next character is a minus
           -> series of numbers is defined*/
         if ( pcstring[li] == '-' )
            {
            li++;
            /*whitespaces are ignored*/
            if ( isspace( pcstring[li] ) )
               {
               li++;
               }
            /*if next number if found ...*/
            if ( isdigit( pcstring[li] ) )
               {
               /*the number is used as
                 the end of the series*/
               sscanf( ( pcstring + li ), "%ld",
                       &lendnumber );
               /*the start of the series is the number
                 that was just added into the list*/
               lstartnumber = pelement->lnumber;
               /*move ahead in the string until the
                 end of the number*/
               ln = ( long ) ( log10 ( (double) ( lendnumber ) ) );
               li += ln + 1;

               /*if there is nothing different defined
                 all numbers between start and and are
                 added to the list*/
               lraise = 1;
               /*whitespaces are ignored*/
               if ( isspace( pcstring[li] ) )
                  {
                  li++;
                  }
               /*if next char is a slash
                 -> raise must be changed to ...*/
               if ( pcstring[li] == '/' )
                  {
                  li++;
                  /*whitespaces are ignored*/
                  if ( isspace( pcstring[li] ) )
                     {
                     li++;
                     }
                  /*... the following number ...*/
                  if ( isdigit( pcstring[li] ) )
                     {
                     sscanf( ( pcstring + li ), "%ld",
                             &lraise );
                     }
                  /*and it needs to be moved ahead
                    until the end of the number*/
                  ln = ( long ) ( log10 (( double ) ( lraise )));
                  li += ln + 1;
                  }

               /*now all desired elements between start
                 and end are added to the list*/
               for ( lj = lstartnumber; lj <= lendnumber - lraise;
                     lj += lraise )
                  {
                  /*allocate element*/
                  pelement = ( list * ) malloc( sizeof ( list ) );
                  /*create an element with the number
                    (startnumber is already in the list!)*/
                  pelement->lnumber = lj + lraise;
                  /*insert created element */
                  pelement->pnext = pfirst;
                  pfirst = pelement;
                  /*and keep in mind that an element was inserted*/
                  ( *picount )++;
                  }
               }
            }
         }
         /*if no number is found -> go on in the string */
      else
         {
         li++;
         }
      }
   /*debugging level 1: mark begin and end of function */
   if ( DEBUGLEVEL > 0)
      {
      printf( "completed function parser\n" );
      fflush(stdout);
      }

   /*return the pointer that points to the start of the list */
   return pfirst;
   }

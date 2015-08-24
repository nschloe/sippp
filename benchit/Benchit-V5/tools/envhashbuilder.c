/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.3 $
* $Date: 2007/01/12 10:44:35 $
* $State: Exp $
*
***********************************************************************/

/** @file envhashbuilder.c
* @Brief creates the environmenthash-sourcecode-file out of a template.
*/

/**
* for file work
*/
#include <stdio.h>
/**
* for string work
*/
#include <string.h>

/**
* BenchIT for advanced string work
*/
#include "stringlib.h"

int isEnvEntry( const char * );

/**
* Brief create the environmenthash-sourcecode-file out of a template.
* Creates bienvhash.c and BIEnvHash.java out of the template by adding
* the environment variables and their settings at the and and appending
* a closing bracket
* @param(in) argc number of arguments given by commandline
* @param(in) argv arguments given by command line
* @returns 0, if succesful
*/
int main( int argc, char **argv )
{
   FILE *efp, *tfp, *ofp, *tfpj, *ofpj;
   char buf[100000];
   char line[STR_LEN], key[STR_LEN], value[STR_LEN], outLine[STR_LEN];
   /* the filenames still hard coded so far */
   const char *envFileName = "tmp.env";
   const char *templateFileName = "bienvhash.template.c";
   const char *outFileName = "../bienvhash.c";
   const char *templateFileNameJava = "BIEnvHash.template.java";
   const char *outFileNameJava = "../jbi/BIEnvHash.java";

   if ( ( efp = fopen( envFileName, "r" ) ) == NULL )
   {
      fprintf( stderr, "File %s couldn't be opened for reading!\n",
         envFileName );
      return 1;
   }
   if ( ( ofp = fopen( outFileName, "w" ) ) == NULL )
   {
      fprintf( stderr, "File %s couldn't be opened for writing!\n",
         outFileName );
      return 1;
   }
   if ( ( ofpj = fopen( outFileNameJava, "w" ) ) == NULL )
   {
      fprintf( stderr, "File %s couldn't be opened for writing!\n",
         outFileNameJava );
      return 1;
   }

   /* create bienvhash.c */
   if ( ( tfp = fopen( templateFileName, "r" ) ) == NULL )
   {
      fprintf( stderr, "File %s couldn't be opened for reading!\n",
         templateFileName );
      return 1;
   }
   /* copy template content into bienvhash.c */
   buf[0] = 0;
   while ( fgets( buf, sizeof( buf ) - 1, tfp ) != (char *)0 )
   {
      fprintf( ofp, "%s", buf );
   }
   fclose( tfp );
   /* create BIEnvHash.java */
   if ( ( tfpj = fopen( templateFileNameJava, "r" ) ) == NULL )
   {
      fprintf( stderr, "File %s couldn't be opened for reading!\n",
         templateFileNameJava );
      return 1;
   }
   /* copy template content into bienvhash.c */
   buf[0] = 0;
   while ( fgets( buf, sizeof( buf ) - 1, tfpj ) != (char *)0 )
   {
      fprintf( ofpj, "%s", buf );
   }
   fclose( tfpj );

   /* append fillTable code */
   while ( fgets( line, STR_LEN, efp ) != NULL )
   {
      if ( isEnvEntry( line ) )
      {
         int eqPos = indexOf( line, '=', 0 );
         /* remove EOL at end of line */
         if ( line[length( line )] == '\n' )
         {
            substring( line, line, 0, length( line ) );
         }
         /* extract key and value for hashtable */
         substring( line, key, 0, eqPos );
         substring( line, value, eqPos + 1, length( line ) + 1 );
         /* remove leading and trailing " and ' */
         trimChar( value, value, '\'' );
         trimChar( value, value, '"' );
         /* replace all occurances of \ by \\ */
         escapeChar( value, value, '\\' );
         /* replace all occurances of " by \" */
         escapeChar( value, value, '"' );
         /* append to output file */
         sprintf( outLine, "   bi_put( \"%s\", \"%s\" );\n", key, value );
         fprintf( ofp, "%s", outLine );
         fprintf( ofpj,"   %s", outLine );
         fflush( stderr );
         fflush( ofp );
         fflush( ofpj );
      }
   }
   /* fileversion code here */
   /* very last line closing the function */
   sprintf( outLine, "}\n" );
   fprintf( ofp, "%s", outLine );
   fprintf( ofpj, "   %s%s", outLine, outLine );
   fflush( stderr );
   fflush( ofp );
   fflush( ofpj );
   fclose( ofp );
   fclose( ofpj );
   fclose( efp );
   return 0;
}

/** Brief check fo is environment entry.
* Checks if a given string is an environment variable
* @param(in) line is this an environment entry? check this input
* @returns 1, if the line starts with a letter and it contains a '='
*/
int isEnvEntry( const char *line )
{
   int retval = 0;
   if ( line == 0 ) return retval;
   /* check for letter and equals */
   if ( ( line[0] >= 'A' ) && ( line[0] <= 'z' )
        && indexOf( line, '=', 0 ) > 0 )
   {
      retval = 1;
   }
   return retval;
}
/*****************************************************************************
Log-History

$Log: envhashbuilder.c,v $
Revision 1.3  2007/01/12 10:44:35  molka
replaced //-comments and tabs

Revision 1.2  2006/09/29 14:50:41  rschoene
commented

Revision 1.1.1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

Revision 1.2  2005/07/04 12:10:38  wloch
inserted CVS comments and prepended bi to hashtable function names

*****************************************************************************/

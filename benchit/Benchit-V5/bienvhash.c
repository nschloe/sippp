/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.12 $
* $Date: 2007/07/06 14:14:47 $
* $State: Exp $
*
***********************************************************************/
/** @file bienvhash.template.c
* @Brief used as hash tables for environment variables from compile time.
* This is a template and will be build to bienvhash.c by adding some bi_put()s
* and a closing bracket.
*/

/**
* used for file works and printing
*/
#include <stdio.h>
/**
* used for typeconversion e.g. atoi()
*/
#include <stdlib.h>
/**
* used for string-works
*/
#include <string.h>

/**
* used for specific types
*/
#include <ctype.h>

/**
* BenchIT: used for more stringwork
*/
#include "tools/stringlib.h"
#include "tools/stringlib.c"

/**
* BenchIT: header for this file
*/
#include "tools/bienvhash.h"

/**
* EMPTY means not found. makes it easier to read
*/
#define EMPTY NULL

/**
* shorter type name
*/
typedef unsigned int us_int;

/*!@brief defines one element in the hashtable
 */
typedef struct element
{
  /*@{*/
  /*!@brief key of this hash-value (remember. e.g. the name). */
  char *key;
  /*@{*/
  /*!@brief length of the name. */
  int keyLength;
  /*@{*/
  /*!@brief value of this hash-entry. */
  char *value;
  /*@{*/
  /*!@brief length of the value. */
  int valueLength;
  /*@{*/
  /*!@brief pointer to the next element. */
  struct element *next;
} ELEMENT;


int HASH_PRIME;
  /*@{*/
  /*!@brief the number of entries in the hash table. */
int ENTRIES;

  /*@{*/
  /*!@brief the hash table itself. */
ELEMENT **table;


/* internal implementations */
void bi_insert( ELEMENT, ELEMENT *[] );
void bi_find( char *, int, ELEMENT *[], ELEMENT**, int * );
us_int bi_hash( char *, int );

/* visible implementations */
void bi_dumpTable(void);
void bi_dumpTableToFile( FILE ** );
char *bi_get ( const char *, int * );
void bi_initTable(void);
int bi_put( const char *, const char * );
int bi_size(void);
int isEnvEntry( const char *line );
int bi_readParameterFile( const char * );
/* special case: generated from outside and code will be
   appended to this file */
void bi_fillTable(void);

/* IMPLEMENTATIONS */


/*!@brief Inserts an element into the table.
 *
 * @param[in] x an element which shall be inserted to the hashtable.
 * @param[in|out] tab the hashtable, where the element shal be inserted to.
 */
void bi_insert( ELEMENT x, ELEMENT *tab[] )
{
  /* chain length at a specific position in the table */
  int chain_length = 0;
  /* hash index for element */
  int ind;
  /* pointer to element x */
  ELEMENT *el;
  /* compute hash index */
  ind = bi_hash( x.key, x.keyLength );
  /* there is no following element, because its the last inserted  */
  x.next = EMPTY;
  if ( tab[ind] == EMPTY )
  {
      /* no entry yet -> create new chain */
    if ( ( tab[ind] = (ELEMENT *)malloc( sizeof( ELEMENT ) ) ) != NULL )
    {
      memcpy( tab[ind], &x, sizeof( x ) );
    }
    else
    {
      exit( 1 );
    }
  }
  else
  {
      /* find end of the chain */
    for ( el = tab[ind]; el->next != EMPTY; el = el->next )
    {
      chain_length++;
    }
      /* append at the end */
    if ( ( el->next = (ELEMENT *)malloc( sizeof( ELEMENT ) ) ) != NULL )
    {
      memcpy( el->next, &x, sizeof( x ) );
    }
    else
    {
      exit( 1 );
    }
  }
}

/*!@brief look for element with key "key" in "tab".
 *         if found: return ( x, ind )
 *        else      return ( x = NULL, ind = ?? )
 *
 * @param[in] key The hash-key for that value.
 * @param[in] len length of the hash-key
 * @param[in|out] tab the hash table to find element with hash-key key
 * @param[out] x the element with hash-key key
 * @param[out] pos the position of x in the hash-table
 */
void bi_find( char *key, int len, ELEMENT *tab[], ELEMENT **x, int *pos )
{
  us_int ind;
  ELEMENT *el;
  /* get hash-index */
  ind = bi_hash( key, len );
  *pos = ind;

   /* look up element in chain */
  for ( el = tab[ind]; el != EMPTY; el = el->next )
  {
    if ( compare( el->key, key ) == 0 )
    {
      /* found */
      *x = el;
      return;
    }
  }
  /* not found */
  *x = NULL;
}

/*!@brief Calculates the index in the table for a given key.
 *
 * @param[in] key The hash-key to compute index for
 * @param[in] len length of the hash-key
 */
us_int bi_hash( char *key, int kl )
{
  us_int help;
  int i;
  /* if its still 0 */
  if ( HASH_PRIME == 0 )
  {
    /* its not initialized yet */
    /* do it! */
    bi_initTable();
  }
   /* calculate a key out of the first 7 characters or less. */
  help = toupper( key[0] ) - 'A' + 1;
   /* for ( i = 0; i < min( 7, kl ); i++ ) */
  for ( i = 0; i < kl; i++ )
  {
    help = help + 27 * (char)toupper( key[i] ) - 'A' + 1;
  }
   /* to avoid storing strings of different length but with
      the same beginning at the same place */
  help = help + kl;
   /* simple hash function */
  return ( help % HASH_PRIME );
}

/*!@brief Dumps table to standard out.
 *
 */
void bi_dumpTable(void)
{
  int i, chained, totalChainLength;
  double load_factor;
  double avg_chain_length;
  ELEMENT *ptr;
  printf( "\nHashtable dump of all known environment variables at compiletime:" );
  printf( "\nIndex |  Key[Length]  | Value[Length]" );
  printf( "\n-------------------------------------" );
  chained = 0;
  totalChainLength = 0;
  for ( i = 0; i < HASH_PRIME; i++ )
  {
    if ( table[i] != EMPTY )
    {
      int count = 0;
      ptr = table[i];
      while ( ptr != NULL )
      {
        int key_length=length(ptr->key)+1;
        int value_length=length(ptr->value)+1;
        printf( "\n  %d  | %s[%d] |  %s[%d]", i, ptr->key,
          key_length, ptr->value , value_length );
        ptr = ptr->next;
        count++;
      }
      if ( count > 1 )
      {
        chained++;
        totalChainLength += count;
      }
    }
  }
  load_factor = 1.0*bi_size()/HASH_PRIME;
  avg_chain_length = 1.0*totalChainLength/chained;
  printf( "\n%d entries in total. Load factor: %f. Indexed chains: %d. "
    "Avg chain length: %f.\n",
    bi_size(), load_factor , chained, avg_chain_length );
  fflush( stdout );
}

/*!@brief Dumps table to output stream. (e.g. bit-file)
 * @param(in) bi_out pointer to the output stream, where the table shall be dumped to
 */
void bi_dumpTableToFile( FILE **bi_out )
{
   int i = 0;
   char buf[100000];
  ELEMENT *ptr = 0;
   memset( buf, 0, 100000 );
   sprintf( buf, "beginofenvironmentvariables\n" );
   bi_fprintf( *bi_out, buf );
   /* all possible entries in hashtable */
   for ( i = 0; i < HASH_PRIME; i++ )
   {
       /* if the position in the hash table is not empty */
      if ( table[i] != EMPTY )
      {
        /* ptr is the element in this table (with key, value, ...) */
         ptr = table[i];
         /* and it exists */
         while ( ptr != NULL )
         {
             /* get the value */
            char *value = bi_strdup( ptr->value ), vbuf[100000];
            /* and its length */
            int vlen = strlen( value ), j = 0, vpos = 0;
            /* delete buffer */
            memset( vbuf, 0 , 100000 );
            
            for ( j = 0; j < vlen; j++ )
            {
               if ( value[j] == '"' )
               {
                  /* check equal number of escape chars->insert escape char */
                  int cnt = 0, k = 0;
                  for ( k = j - 1;  k >= 0; k-- )
                  {
                     if ( value[k] == '\\' ) cnt++;
                  }
                  /* escape quote */
                  /* this is only changed in vbuf, not in value, so it counts the correct */
                  /* number of \\s */
                  if ( cnt % 2 == 0 )
                  {
                     vbuf[vpos++] = '\\';
                  }
               }
               vbuf[vpos++] = value[j];
            }
            /* end buffer */
            vbuf[vpos++] = '\0';
            /* print it to file */
            sprintf( buf, "%s=\"%s\"\n", ptr->key, vbuf );
            bi_fprintf( *bi_out, buf );
            /* next element at this position of hashtable */
            ptr = ptr->next;
         }
      }
   }
   sprintf( buf, "endofenvironmentvariables\n" );
   bi_fprintf( *bi_out, buf );
}

/*!@brief Retrieves a value from the table. If the given key
 *   does not exist a null pointer is returned. valueLength
 *   will represent the length of the returned value.
 * @param(in) key String, containing the key to search for
 * @param(out) valuLength length of the returned value for key
 * @returns the value for the key or NULL if not found
 */
char *bi_get ( const char *key, int *valueLength )
{
  char *retval = 0;
  int i;
  ELEMENT *el;

  *valueLength = 0;
  /* find the value */
  bi_find( (char *)key, length( key ) +1, table, &el, &i );
  /* does it exist? */
  if ( el != NULL )
  {
    /* compute length */
    retval = el->value;
    *valueLength = el->valueLength;
  }
  return retval;
}

/*!@brief Creates the table and initializes the fields.
 */
void bi_initTable(void)
{
  int i;
  /* set the hash prime */
  HASH_PRIME = 1009;
  ENTRIES = 0;
  /* allocate memory */
  table = (ELEMENT **)malloc( HASH_PRIME * sizeof( ELEMENT * ) );
   /* mark all entries as FREE */
  for ( i = 0; i < HASH_PRIME; i++ ) table[i] = EMPTY;
}

/*!@brief Puts a Key-Value pair into the table. If the key
 *   already exists, the value will be overwritten.
 *   Returns 0, if the key is new, 1 if a value was
 *   overwritten, and -1 if an error occured.
 * @param(in) key the key for a value, that shall be inserted into table
 * @param(in) value the value for this key
 * returns 0 if the key is new,
 * 1 if the value for this key is overwritten
 * and -1 if an error occured
 */
int bi_put( const char *key, const char *value )
{
  int retval = -1, kLen = -1, vLen = -1, j = -1;
  /* needed, if element is found in table */
  ELEMENT *e = NULL;
  /* is just needed, if the element is new */
  ELEMENT el;
  if ( ( key == 0 ) || ( value == 0 ) ) return retval;
   /* some environment variables end on dead characters (0x11 and 0x19)
    * which will be eliminated now. */
  kLen = length( key ) + 1;
  vLen = length( value ) + 1;
  IDL(5,printf("try to find %s and set to new value %s ...",key,value));
   /* get or create new element */
  bi_find( (char *)key, kLen, table, &e, &j );
  if ( e != NULL )
  {
    retval = 1;
    IDL(5,printf("found. old is %s\n",e->value));
  }
  else
  {
    retval = 0;
    IDL(5,printf("not found\n"));
  }
  /* if the key exists */
  if  (retval==1)
  {
    /* the value should also exist, but check it */
    if ( e->value != NULL )
    {
      /* and set it free */
      free( e->value );
    }
    /* create the new value, which is one larger then the input by allocating */
    if ( ( e->value = (char *)malloc( (vLen+1) * sizeof( char ) ) ) != NULL )
    {
      /* copy the value given by the function call to the value in the element */
      strncpy( e->value, value, vLen );
      /* set last char to '\0' in case src string was longer */
      e->value[vLen] = '\0';
      /* set lengths */
      e->valueLength = vLen;
    }
    else
    {
      /* if malloc wasn't succesful */
      IDL(-1,printf("Creation NOT succesfull"));
    }
    return retval;
  }
   /* set key if key is new */
  memset( &el, 0, sizeof( ELEMENT ) );
  if ( el.key == 0 )
  {
    if ( ( el.key = (char *)malloc( (kLen+1) * sizeof( char ) ) ) != NULL )
    {
      strncpy( el.key, key, kLen+1 );
      /* set last char to '\0' in case src string was longer */
      el.key[kLen] = '\0';
      el.keyLength = kLen;
    }
    else
    {
      return -1;
    }
  }
   /* set or replace value */
  if ( ( el.value = (char *)malloc( (vLen+1) * sizeof( char ) ) ) != NULL )
  {
    strncpy( el.value, value, vLen );
    /* set last char to '\0' in case src string was longer */
    el.value[vLen] = '\0';
    el.valueLength = vLen;
  }
  else
  {
    if ( ( retval == 0 ) && ( el.key != NULL ) )
    {
      free( el.key );
    }
    return -1;
  }
   /* insert the new element; if the key existed, the element was
      already updated by the above code */
  if ( retval == 0 )
  {
    bi_insert( el, table );
    ENTRIES++;
  }
  return retval;
}

/*!@brief Returns the number of entries stored in the table.
 * @returns number of entries in table
*/
int bi_size(void)
{
  return ENTRIES;
}

/*!@brief internal check for Strings (see returnvalue)
 * @returns 1, if the line starts with a letter, and if it contains an equals
 *   sign and no '$', 0 otherwise.
 */
int isEnvEntry( const char *line )
{
  int retval = 0;
  if ( line == 0 ) return retval;
  /* starts with a uppercase letter? */
  if ( ( line[0] >= 'A' ) && ( line[0] <= 'Z' )
  /* and contains an equal sign and a $? */
    && indexOf( line, '=', 0 ) > 0 && indexOf( line, '$', 0 ) < 0 )
  {
    retval = 1;
  }
  return retval;
}

/*!@brief Takes a PARAMETER file as argument and adds it's variables
 * to the environment hash table.
 * @param fileName name of the file, which contains the parameters to read 
 * @returns 1 on success, 0 else.
 */
int bi_readParameterFile( const char *fileName )
{
  FILE *efp = 0;
  char line[STR_LEN], key[STR_LEN], value[STR_LEN];

  memset( line, 0, STR_LEN );
  memset( key, 0, STR_LEN );
  memset( value, 0, STR_LEN );
  if ( ( efp = fopen( fileName, "r" ) ) == NULL )
  {
    fprintf( stderr, "File %s couldn't be opened for reading!\n",
      fileName );
    return 0;
  }

  while ( fgets( line, STR_LEN, efp ) != NULL )
  {
      /* remove leading and trailing white spaces */
    trim( line, line );
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
      bi_put( key, value );
    }
  }
  return 1;
}

/* special case: generated from outside and code will be
   appended to this file */
/*!@brief Fills the table with predefined content. */
void bi_fillTable(void)
{
/*****************************************************************************
Log-History

$Log: bienvhash.template.c,v $
Revision 1.12  2007/07/06 14:14:47  rschoene
moved declarations

Revision 1.11  2007/01/25 14:50:11  molka
removed some icc warnings

Revision 1.10  2007/01/19 15:05:11  molka
removed some icc-warnings

Revision 1.9  2007/01/12 10:44:35  molka
replaced //-comments and tabs

Revision 1.8  2006/11/23 07:14:02  rschoene
removed non-working debug-print

Revision 1.7  2006/11/20 14:35:25  rschoene
changed sth in bi_put (better comments divided el and e)

Revision 1.6  2006/09/29 14:32:44  rschoene
commented

Revision 1.5  2006/08/09 07:01:52  rschoene
if put(k,v) is called and k already exists, its value will be set to v

Revision 1.4  2005/11/29 20:07:36  mickler
# Removed unused variables

Revision 1.3  2005/07/21 13:32:04  wloch
implemented hashtable dump into bit file

Revision 1.2  2005/07/19 12:17:59  wloch
added cvs footer

Revision 1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

Revision 1.2  2005/07/04 12:10:38  wloch
inserted CVS comments and prepended bi to hashtable function names

*****************************************************************************/
   bi_put( "ANT_HOME", "/usr/share/ant-core" );
   bi_put( "BASH", "/bin/sh" );
   bi_put( "BASH_ARGC", "([0]=\"1\" [1]=\"1\")" );
   bi_put( "BASH_ARGV", "([0]=\"/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/kernel/numerical/rkstep/F95/0/0/gauss2/COMPILE.SH\" [1]=\"numerical.rkstep.F95.0.0.gauss2\")" );
   bi_put( "BASH_LINENO", "([0]=\"78\" [1]=\"0\")" );
   bi_put( "BASH_SOURCE", "([0]=\"/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/kernel/numerical/rkstep/F95/0/0/gauss2/COMPILE.SH\" [1]=\"/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/COMPILE.SH\")" );
   bi_put( "BASH_VERSINFO", "([0]=\"3\" [1]=\"2\" [2]=\"17\" [3]=\"1\" [4]=\"release\" [5]=\"i686-pc-linux-gnu\")" );
   bi_put( "BASH_VERSION", "3.2.17(1)-release" );
   bi_put( "BENCHITDIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "BENCHITROOT", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "BENCHIT_ARCH_SHORT", "IntM" );
   bi_put( "BENCHIT_ARCH_SPEED", "600M" );
   bi_put( "BENCHIT_CC", "icc" );
   bi_put( "BENCHIT_CC_COMPILER_VERSION", "910, build 20061103" );
   bi_put( "BENCHIT_CC_C_FLAGS", "" );
   bi_put( "BENCHIT_CC_C_FLAGS_HIGH", "-O3" );
   bi_put( "BENCHIT_CC_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_CC_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_CC_L_FLAGS", "-lm -L/opt/intel/fortran100/lib -lifcore" );
   bi_put( "BENCHIT_COMPILER", "ifort" );
   bi_put( "BENCHIT_COMPILERFLAGS", "-free  -O4" );
   bi_put( "BENCHIT_CPP_ACML", "" );
   bi_put( "BENCHIT_CPP_ATLAS", "" );
   bi_put( "BENCHIT_CPP_BLAS", "" );
   bi_put( "BENCHIT_CPP_ESSL", "" );
   bi_put( "BENCHIT_CPP_FFTW3", "" );
   bi_put( "BENCHIT_CPP_MKL", "" );
   bi_put( "BENCHIT_CPP_MPI", " -DUSE_MPI" );
   bi_put( "BENCHIT_CPP_PAPI", "-DUSE_PAPI" );
   bi_put( "BENCHIT_CPP_PCL", " -DUSE_PCL" );
   bi_put( "BENCHIT_CPP_PTHREADS", "" );
   bi_put( "BENCHIT_CPP_PVM", "" );
   bi_put( "BENCHIT_CPP_SCSL", "" );
   bi_put( "BENCHIT_CROSSCOMPILE", "0" );
   bi_put( "BENCHIT_CXX", "c++" );
   bi_put( "BENCHIT_CXX_COMPILER_VERSION", "4.1.2 (Gentoo 4.1.2 p1.0.1)" );
   bi_put( "BENCHIT_CXX_C_FLAGS", "" );
   bi_put( "BENCHIT_CXX_C_FLAGS_HIGH", "-O3" );
   bi_put( "BENCHIT_CXX_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_CXX_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_CXX_L_FLAGS", "-lm" );
   bi_put( "BENCHIT_C_COMPILERFLAGS", " -O2" );
   bi_put( "BENCHIT_DEBUGLEVEL", "0" );
   bi_put( "BENCHIT_DEFINES", " -DDEBUGLEVEL=0" );
   bi_put( "BENCHIT_EDITOR", "/usr/bin/vim" );
   bi_put( "BENCHIT_ENVIRONMENT", "NOTHING" );
   bi_put( "BENCHIT_F77", "ifort" );
   bi_put( "BENCHIT_F77_COMPILER_VERSION", " unknown" );
   bi_put( "BENCHIT_F77_C_FLAGS", "" );
   bi_put( "BENCHIT_F77_C_FLAGS_HIGH", "-O3" );
   bi_put( "BENCHIT_F77_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_F77_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_F77_L_FLAGS", "-lm" );
   bi_put( "BENCHIT_F90", "ifort" );
   bi_put( "BENCHIT_F90_COMPILER_VERSION", " unknown" );
   bi_put( "BENCHIT_F90_C_FLAGS", "-free" );
   bi_put( "BENCHIT_F90_C_FLAGS_HIGH", "-O3" );
   bi_put( "BENCHIT_F90_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_F90_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_F90_L_FLAGS", "-lm" );
   bi_put( "BENCHIT_F90_SOURCE_FORMAT_FLAG", "" );
   bi_put( "BENCHIT_F95", "ifort" );
   bi_put( "BENCHIT_F95_COMPILER_VERSION", " unknown" );
   bi_put( "BENCHIT_F95_C_FLAGS", "-free" );
   bi_put( "BENCHIT_F95_C_FLAGS_HIGH", "-O4" );
   bi_put( "BENCHIT_F95_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_F95_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_F95_L_FLAGS", "-lm" );
   bi_put( "BENCHIT_F95_SOURCE_FORMAT_FLAG", "" );
   bi_put( "BENCHIT_FILENAME_COMMENT", "0" );
   bi_put( "BENCHIT_F_COMPILERFLAGS", "-free  -O4" );
   bi_put( "BENCHIT_HOSTNAME", "localhost" );
   bi_put( "BENCHIT_IGNORE_PARAMETER_FILE", "0" );
   bi_put( "BENCHIT_INCLUDES", "-I. -I/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "BENCHIT_INTERACTIVE", "0" );
   bi_put( "BENCHIT_JAVA", "java" );
   bi_put( "BENCHIT_JAVAC", "javac" );
   bi_put( "BENCHIT_JAVAC_FLAGS", "" );
   bi_put( "BENCHIT_JAVAC_FLAGS_HIGH", "-O" );
   bi_put( "BENCHIT_JAVA_FLAGS", "" );
   bi_put( "BENCHIT_JAVA_HOME", "" );
   bi_put( "BENCHIT_KERNELBINARY", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/bin/numerical.rkstep.F95.0.0.gauss2.0" );
   bi_put( "BENCHIT_KERNELBINARY_ARGS", " " );
   bi_put( "BENCHIT_KERNELNAME", "numerical.rkstep.F95.0.0.gauss2" );
   bi_put( "BENCHIT_KERNEL_INCREMENT", "1" );
   bi_put( "BENCHIT_KERNEL_START", "2" );
   bi_put( "BENCHIT_KERNEL_STEPS", "1000" );
   bi_put( "BENCHIT_LD_LIBRARY_PATH", "/home/nico/Desktop/Diplomarbeit/programs/main/parabolic-1D/Benchit-V5/jbi/jni" );
   bi_put( "BENCHIT_LIB_ACML", " -lacml" );
   bi_put( "BENCHIT_LIB_ATLAS", " -latlas" );
   bi_put( "BENCHIT_LIB_BLAS", "-lblas" );
   bi_put( "BENCHIT_LIB_ESSL", " -lessl" );
   bi_put( "BENCHIT_LIB_FFTW3", " -lfftw3" );
   bi_put( "BENCHIT_LIB_MKL", " -lmkl" );
   bi_put( "BENCHIT_LIB_MPI", "" );
   bi_put( "BENCHIT_LIB_PAPI", "" );
   bi_put( "BENCHIT_LIB_PCL", "" );
   bi_put( "BENCHIT_LIB_PTHREAD", "-lpthread" );
   bi_put( "BENCHIT_LIB_PVM", "" );
   bi_put( "BENCHIT_LIB_SCSL", " -lscsl" );
   bi_put( "BENCHIT_MANDATORY_FILES", "tools/run_benchit tools/uname_minus_a benchit.c interface.h tools/envhashbuilder.c tools/bienvhash.template.c tools/bienvhash.h tools/stringlib.c tools/stringlib.h " );
   bi_put( "BENCHIT_MPICC", "icc" );
   bi_put( "BENCHIT_MPICC_C_FLAGS", "" );
   bi_put( "BENCHIT_MPICC_C_FLAGS_HIGH", "-O3" );
   bi_put( "BENCHIT_MPICC_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_MPICC_C_FLAGS_STD", "-O2" );
   bi_put( "BENCHIT_MPICC_L_FLAGS", "-lm -L/opt/intel/fortran100/lib -lifcore -lmpi" );
   bi_put( "BENCHIT_MPIF77", "" );
   bi_put( "BENCHIT_MPIF77_C_FLAGS", "" );
   bi_put( "BENCHIT_MPIF77_C_FLAGS_HIGH", "" );
   bi_put( "BENCHIT_MPIF77_C_FLAGS_OMP", "" );
   bi_put( "BENCHIT_MPIF77_C_FLAGS_STD", "" );
   bi_put( "BENCHIT_MPIF77_L_FLAGS", "" );
   bi_put( "BENCHIT_MPIRUN", "mpirun" );
   bi_put( "BENCHIT_NODENAME", "N4" );
   bi_put( "BENCHIT_NUM_CPUS", "1" );
   bi_put( "BENCHIT_NUM_PROCESSES", "" );
   bi_put( "BENCHIT_NUM_THREADS_PER_PROCESS", "" );
   bi_put( "BENCHIT_OPTIONAL_FILES", "LOCALDEFS/PROTOTYPE_input_architecture LOCALDEFS/PROTOTYPE_input_display " );
   bi_put( "BENCHIT_PARAMETER_FILE", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/kernel/numerical/rkstep/F95/0/0/gauss2/PARAMETERS" );
   bi_put( "BENCHIT_PERL_FLAGS", "" );
   bi_put( "BENCHIT_PERL_INTERPRETER", "" );
   bi_put( "BENCHIT_PHP_FLAGS", "" );
   bi_put( "BENCHIT_PHP_INTERPRETER", "" );
   bi_put( "BENCHIT_PROGRESS_DIR", "progress" );
   bi_put( "BENCHIT_RUBY_FLAGS", "" );
   bi_put( "BENCHIT_RUBY_INTERPRETER", "" );
   bi_put( "BENCHIT_RUN_ACCURACY", "" );
   bi_put( "BENCHIT_RUN_COREDUMPLIMIT", "0" );
   bi_put( "BENCHIT_RUN_EMAIL_ADDRESS", "" );
   bi_put( "BENCHIT_RUN_LINEAR", "0" );
   bi_put( "BENCHIT_RUN_MAX_MEMORY", "0" );
   bi_put( "BENCHIT_RUN_OUTPUT_DIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/output" );
   bi_put( "BENCHIT_RUN_QUEUENAME", "" );
   bi_put( "BENCHIT_RUN_QUEUEOPTIONS", "" );
   bi_put( "BENCHIT_RUN_QUEUETIMELIMIT", "" );
   bi_put( "BENCHIT_RUN_REDIRECT_CONSOLE", "output.txt" );
   bi_put( "BENCHIT_RUN_TEST", "0" );
   bi_put( "BENCHIT_RUN_TIMELIMIT", "600" );
   bi_put( "BENCHIT_SHELL_FLAGS", "" );
   bi_put( "BENCHIT_SHELL_INTERPRETER", "" );
   bi_put( "BENCHIT_USE_VAMPIR_TRACE", "0" );
   bi_put( "BR", "0" );
   bi_put( "CG_COMPILER_EXE", "/usr/bin/cgc" );
   bi_put( "CLASSPATH", "." );
   bi_put( "COLON_SEPARATED", "XDG_DATA_DIRS" );
   bi_put( "COLORTERM", "" );
   bi_put( "COMPILED_SUCCESSFULLY", "0" );
   bi_put( "COMPILE_GLOBAL", "1" );
   bi_put( "CONFIGURE_MODE", "COMPILE" );
   bi_put( "CONFIG_PROTECT", "/usr/share/X11/xkb /usr/kde/3.5/share/config /usr/kde/3.5/env /usr/kde/3.5/shutdown /usr/share/config" );
   bi_put( "CONFIG_PROTECT_MASK", "/etc/env.d/java/ /etc/gconf /etc/terminfo /etc/revdep-rebuild /etc/splash" );
   bi_put( "CURDIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "CVS_RSH", "ssh" );
   bi_put( "C_COMPILE", "icc  -O2 -I. -I/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5  -DDEBUGLEVEL=0" );
   bi_put( "DBUS_SESSION_BUS_ADDRESS", "unix:abstract=/tmp/dbus-4M4lTOyQbA,guid=dbe63a10819768452d5cac0046f80258" );
   bi_put( "DCCC_PATH", "/usr/lib/distcc/bin" );
   bi_put( "DESKTOP_SESSION", "default" );
   bi_put( "DIRSTACK", "()" );
   bi_put( "DISPLAY", ":0.0" );
   bi_put( "DISTCC_LOG", "" );
   bi_put( "DISTCC_VERBOSE", "0" );
   bi_put( "DM_CONTROL", "/var/run/xdmctl" );
   bi_put( "EDITOR", "/usr/bin/vim" );
   bi_put( "EUID", "1000" );
   bi_put( "FLTK_DOCDIR", "/usr/share/doc/fltk-1.1.7-r2/html" );
   bi_put( "F_COMPILE", "ifort -free  -O4" );
   bi_put( "GCC_PATH", "/usr/i686-pc-linux-gnu/gcc-bin/4.1.2" );
   bi_put( "GCC_SPECS", "" );
   bi_put( "GDK_USE_XFT", "1" );
   bi_put( "GENERATION", "2" );
   bi_put( "GROUPS", "()" );
   bi_put( "GS_LIB", "/home/nico/.fonts" );
   bi_put( "GTK2_RC_FILES", "/home/nico/.gtkrc-2.0:/home/nico/.kde/share/config/gtkrc-2.0:/etc/gtk-2.0/gtkrc" );
   bi_put( "GTK_RC_FILES", "/etc/gtk/gtkrc:/home/nico/.gtkrc:/home/nico/.kde/share/config/gtkrc" );
   bi_put( "G_BROKEN_FILENAMES", "1" );
   bi_put( "G_FILENAME_ENCODING", "UTF-8" );
   bi_put( "HLL", "F95" );
   bi_put( "HOME", "/home/nico" );
   bi_put( "HOSTNAME", "N4" );
   bi_put( "HOSTTYPE", "i686" );
   bi_put( "HTTP_PROXY", "http://localhost:8118" );
   bi_put( "IA32ROOT", "/opt/intel/fortran100" );
   bi_put( "IFS", "' 	" );
   bi_put( "INFOPATH", "/usr/share/info:/usr/share/binutils-data/i686-pc-linux-gnu/2.17/info:/usr/share/gcc-data/i686-pc-linux-gnu/4.1.2/info:/usr/local/texlive/current/texmf/doc/info" );
   bi_put( "INTEL_FLEXLM_LICENSE", "/opt/intel/compiler91/licenses" );
   bi_put( "INTEL_LICENCE_FILE", "/opt/intel/fortran100/licenses" );
   bi_put( "JAVAC", "/home/nico/.gentoo/java-config-2/current-user-vm/bin/javac" );
   bi_put( "JAVACC_HOME", "/usr/share/javacc/" );
   bi_put( "JAVA_HOME", "/home/nico/.gentoo/java-config-2/current-user-vm" );
   bi_put( "JDK_HOME", "/home/nico/.gentoo/java-config-2/current-user-vm" );
   bi_put( "KDEDIRS", "/usr:/usr/local:/usr/kde/3.5" );
   bi_put( "KDE_FULL_SESSION", "true" );
   bi_put( "KDE_MULTIHEAD", "false" );
   bi_put( "KDE_SESSION_UID", "1000" );
   bi_put( "KERNELBASEDIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/kernel" );
   bi_put( "KERNELDIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/kernel/numerical/rkstep/F95/0/0/gauss2" );
   bi_put( "KERNELNAME_FULL", "" );
   bi_put( "KONSOLE_DCOP", "DCOPRef(konsole-30585,konsole)" );
   bi_put( "KONSOLE_DCOP_SESSION", "DCOPRef(konsole-30585,session-1)" );
   bi_put( "LANGUAGE", "en_US.UTF-8" );
   bi_put( "LC_COLLATE", "en_US.UTF-8" );
   bi_put( "LC_CTYPE", "en_US.UTF-8" );
   bi_put( "LC_MESSAGES", "en_US.UTF-8" );
   bi_put( "LC_MONETARY", "de_DE.UTF-8" );
   bi_put( "LC_NUMERIC", "de_DE.UTF-8" );
   bi_put( "LC_PAPER", "de_DE.UTF-8" );
   bi_put( "LC_TIME", "de_DE.UTF-8" );
   bi_put( "LDPATH", "/usr/local/lib://usr//lib/opengl/nvidia/lib:/usr/i686-pc-linux-gnu/lib:/usr/lib/gcc/i686-pc-linux-gnu/4.1.2:/opt/intel/compiler91/lib:/opt/intel/fortran100/lib:/usr/lib/nspr:/usr/lib/nss:/opt/sun-jdk-1.4.2.15/jre/lib/i386/:/opt/sun-jdk-1.4.2.15/jre/lib/i386/native_threads/:/opt/sun-jdk-1.4.2.15/jre/lib/i386/classic/:/opt/sun-jdk-1.4.2.15/jre/lib/i386/server/:/usr/lib/qt4:/usr/kde/3.5/lib:/usr/qt/3/lib:/usr/games/lib:/opt/nessus/lib:/usr/lib/fltk-1.1:/usr/lib/libstdc++-v3/" );
   bi_put( "LD_LIBRARY_PATH", "/opt/sun-jdk-1.6.0.02/jre/lib/i386/client:/opt/sun-jdk-1.6.0.02/jre/lib/i386:/opt/sun-jdk-1.6.0.02/jre/../lib/i386" );
   bi_put( "LESS", "-R -M --shift 5" );
   bi_put( "LESSOPEN", "|lesspipe.sh %s" );
   bi_put( "LOGNAME", "nico" );
   bi_put( "LS_COLORS", "no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.pdf=00;32:*.ps=00;32:*.txt=00;32:*.patch=00;32:*.diff=00;32:*.log=00;32:*.tex=00;32:*.doc=00;32:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:" );
   bi_put( "MACHTYPE", "i686-pc-linux-gnu" );
   bi_put( "MANPATH", "/home/nico/.gentoo/java-config-2/current-user-vm/man:/usr/local/share/man:/usr/share/man:/usr/share/binutils-data/i686-pc-linux-gnu/2.17/man:/usr/share/gcc-data/i686-pc-linux-gnu/4.1.2/man:/opt/intel/compiler91/man:/opt/intel/fortran100/man:/opt/sun-jdk-1.4.2.15/man:/etc/java-config/system-vm/man/:/usr/kde/3.5/share/man:/usr/qt/3/doc/man:/usr/local/texlive/current/texmf/doc/man" );
   bi_put( "MIKTEX_INSTALLROOT", "/usr/local/texlive/texmf-local" );
   bi_put( "NLSPATH", "/usr/dt/lib/nls/msg/%L/%N.cat" );
   bi_put( "OLDCWD", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "OLDIR", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "OLDPWD", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5" );
   bi_put( "OMP_DYNAMIC", "FALSE" );
   bi_put( "OMP_NESTED", "FALSE" );
   bi_put( "OPENGL_PROFILE", "nvidia" );
   bi_put( "OPTERR", "1" );
   bi_put( "OPTIND", "1" );
   bi_put( "OSNAME", "Linux" );
   bi_put( "OSTYPE", "linux-gnu" );
   bi_put( "PAGER", "/usr/bin/less" );
   bi_put( "PATH", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/tools:/usr/kde/3.5/bin:/usr/local/bin:/usr/bin:/bin:/opt/bin:/usr/i686-pc-linux-gnu/gcc-bin/4.1.2:/opt/intel/compiler91/bin:/opt/intel/fortran100/bin:/opt/sun-jdk-1.4.2.15/bin:/opt/sun-jdk-1.4.2.15/jre/bin:/opt/sun-jdk-1.4.2.15/jre/javaws:/usr/kde/3.5/bin:/usr/qt/3/bin:/usr/games/bin:/usr/local/texlive/current/bin/i386-linux:/sbin:/usr/sbin:/sbin:/usr/sbin" );
   bi_put( "PIPESTATUS", "([0]=\"0\")" );
   bi_put( "PKG_CONFIG_PATH", "/usr/qt/3/lib/pkgconfig" );
   bi_put( "POSIXLY_CORRECT", "y" );
   bi_put( "PPID", "11301" );
   bi_put( "PRELINK_PATH", "" );
   bi_put( "PRELINK_PATH_MASK", "/usr/lib/gstreamer-0.10:/lib/modules:/usr/lib/locale:/usr/lib/wine:/usr/lib/valgrind:*.la:*.png:*.py:*.pl:*.pm:*.sh:*.xml:*.xslt:*.a:*.js:/usr/lib/klibc" );
   bi_put( "PS4", "+ " );
   bi_put( "PWD", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/tools" );
   bi_put( "PYTHONPATH", "/usr/lib/portage/pym" );
   bi_put( "QMAKESPEC", "linux-g++" );
   bi_put( "QTDIR", "/usr/qt/3" );
   bi_put( "SCRIPTNAME", "COMPILE.SH" );
   bi_put( "SESSION_MANAGER", "local/N4:/tmp/.ICE-unix/5777" );
   bi_put( "SHELL", "/bin/bash" );
   bi_put( "SHELLOPTS", "braceexpand:hashall:interactive-comments:posix" );
   bi_put( "SHELLSCRIPT_DEBUG", "0" );
   bi_put( "SHLVL", "7" );
   bi_put( "TERM", "xterm" );
   bi_put( "TEXBASE", "/usr/local/texlive/" );
   bi_put( "UID", "1000" );
   bi_put( "USER", "nico" );
   bi_put( "VMHANDLE", "sun-jdk-1.4" );
   bi_put( "WINDOWID", "73400327" );
   bi_put( "XCURSOR_THEME", "whiteglass" );
   bi_put( "XDG_CONFIG_DIRS", "/usr/kde/3.5/etc/xdg" );
   bi_put( "XDG_DATA_DIRS", "/usr/share:/usr/kde/3.5/share:/usr/local/share" );
   bi_put( "XDM_MANAGED", "/var/run/xdmctl/xdmctl-:0,maysd,mayfn,sched,rsvd,method=classic" );
   bi_put( "XFILESEARCHPATH", "/usr/dt/app-defaults/%L/Dt" );
   bi_put( "_", "/home/nico/Uni/Diplomarbeit/programs/fortran/benchit/Benchit-V5/tools/" );
   bi_put( "_CMDLINE_VARLIST", "BENCHIT_KERNELBINARY BENCHIT_KERNELBINARY_ARGS BENCHIT_CMDLINE_ARG_FILENAME_COMMENT BENCHIT_CMDLINE_ARG_PARAMETER_FILE BENCHIT_CMDLINE_ARG_IGNORE_PARAMETER_FILE BENCHIT_NODENAME BENCHIT_CROSSCOMPILE BENCHIT_CMDLINE_ARG_NUM_CPUS BENCHIT_CMDLINE_ARG_NUM_PROCESSES BENCHIT_CMDLINE_ARG_NUM_THREADS_PER_PROCESS BENCHIT_CMDLINE_ARG_RUN_CLEAN BENCHIT_CMDLINE_ARG_RUN_COREDUMPLIMIT BENCHIT_CMDLINE_ARG_RUN_EMAIL_ADDRESS BENCHIT_CMDLINE_ARG_RUN_MAX_MEMORY BENCHIT_CMDLINE_ARG_RUN_QUEUENAME BENCHIT_CMDLINE_ARG_RUN_QUEUETIMELIMIT BENCHIT_CMDLINE_ARG_RUN_REDIRECT_CONSOLE BENCHIT_CMDLINE_ARG_RUN_TEST BENCHIT_CMDLINE_ARG_RUN_USE_MPI BENCHIT_CMDLINE_ARG_RUN_USE_OPENMP " );
   bi_put( "_VARLIST", "'BENCHITROOT" );
   bi_put( "http_proxy", "http://localhost:8118" );
   bi_put( "menuitem_cleanup", "0" );
   bi_put( "menuitem_compile", "0" );
   bi_put( "menuitem_exit", "3" );
   bi_put( "menuitem_gui", "1" );
   bi_put( "menuitem_run", "0" );
   bi_put( "menuitem_selkernel", "2" );
   bi_put( "menuitem_setparams", "0" );
   bi_put( "menuitem_viewresults", "0" );
   bi_put( "myfile", "tools/stringlib.h" );
   bi_put( "myval", "" );
   bi_put( "myvar", "BENCHIT_CMDLINE_ARG_RUN_USE_OPENMP" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_ENVIRONMENTS", "NO REVISION, UNABLE TO READ (Sun Aug 12 00:00:02 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_QUICKVIEW_SH", "1.3 (Fri Sep 29 18:35:11 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_ENVHASHBUILDER", "NO REVISION (Tue Sep 25 00:33:40 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_ARCHDEFS", "NO REVISION (Fri Sep 29 18:35:11 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_CMDLINEPARAMS", "1.29 (Tue Jan  3 18:55:28 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_COMPILERVERSION", "NO REVISION, UNABLE TO READ (Sun Aug 12 00:00:02 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_CONFIGURE", "1.46 (Thu Jun 21 19:41:24 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_LOC_CONVERT_SH", "NO REVISION (Wed Jan  4 15:32:49 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_LOC_REPL", "NO REVISION (Wed Jan  4 15:32:49 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_FIRSTTIME", "1.29 (Wed Jul  4 16:22:09 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BMERGE_SH", "NO REVISION (Mon Jul 18 15:03:19 2005)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_ERROR_H", "1.1 (Tue Mar 13 08:22:03 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BENCHSCRIPT_C", "1.2 (Fri Jan 12 11:44:35 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BENCHSCRIPT_H", "1.1 (Tue May 23 10:13:00 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_UNAME_MINUS_A", "NO REVISION (Mon Jul 18 15:03:19 2005)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_TMP_ENV", "NO REVISION (Tue Sep 25 00:33:44 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_ENVHASHBUILDER_C", "1.3 (Fri Jan 12 11:44:35 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_FILEVERSION", "NO REVISION (Tue Sep 25 00:33:41 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_STRINGLIB_C", "1.4 (Fri Jan 12 11:44:35 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_STRINGLIB_H", "1.2 (Fri Sep 29 16:51:14 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_RANDOM", "NO REVISION, UNABLE TO READ (Sun Aug 12 00:00:02 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_FEATURES", "NO REVISION (Tue Jul 10 13:10:16 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_FILEVERSION_C", "1.6 (Fri Jan 19 16:05:11 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BIENVHASH_TEMPLATE_JAVA", "1.4 (Thu Jul 21 21:42:13 2005)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_RUN_BENCHIT", "NO REVISION (Fri Sep 29 18:35:11 2006)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BIENVHASH_TEMPLATE_C", "1.12 (Fri Jul  6 16:14:47 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_HELPER_SH", "1.14 (Mon Dec 12 11:27:20 2005)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_BIENVHASH_H", "1.5 (Fri Jan 19 16:05:11 2007)" );
   bi_put( "BENCHIT_KERNEL_FILE_VERSION_COMMONDEFS", "NO REVISION (Tue Aug 30 21:32:48 2005)" );
}

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

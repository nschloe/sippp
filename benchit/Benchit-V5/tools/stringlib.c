/***********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: molka $
 * $Revision: 1.4 $
 * $Date: 2007/01/12 10:44:35 $
 ***********************************************************************/
 
/** @file stringlib.c
* @Brief For advanced string work.
*/
/**
* for printf and stuff
*/
#include <stdio.h>
/**
* "classical" string work
*/
#include <string.h>
/**
* the own header
*/
#include "stringlib.h"

int compare( const char *, const char * );
int comparem( const char *, char * );
int comparec( char *, char * );
int escapeChar( const char *, char *, char );
int indexOf( const char *, char, int );
int lastIndexOf( const char *, char, int );
int length( const char * );
int lengthc( char * );
int substring( const char*, char*, int, int );
int trim( const char *, char * );
int trimChar( const char *, char *, char );

int min( int, int );

/**
* Brief compares 2 srings - if unequal length, the first n chars are
* compared - n is equal to the length of the shorter string.
* @param(in) strA first string
* @param(in) strB second string
* @returns 0, if one of these strings is NULL or both are equal
* when they have different lengths: if all chars untils the n-th
* (see description) are equal and strA is shorter 1000000 is returned,
* if the n chars are equal and strB is shorter -1000000 is returned
* @deprecated use comparec(char*, char*) instead
*/
int compare( const char *strA, const char *strB )
{
   return comparec( (char *)strA, (char *)strB );
}

/**
* Brief compares 2 srings - if unequal length, the first n chars are
* compared - n is equal to the length of the shorter string.
* @param(in) strA first string
* @param(in) strB second string
* @returns 0, if one of these strings is NULL or both are equal
* when they have different lengths: if all chars untils the n-th
* (see description) are equal and strA is shorter 1000000 is returned,
* if the n chars are equal and strB is shorter -1000000 is returned
* @deprecated use comparec(char*, char*) instead
*/
int comparem( const char *strA, char *strB )
{
   return comparec( (char *)strA, strB );
}

/**
* Brief compares 2 srings. if unequal length, the first n chars are
* compared. n is equal to the length of the shorter string.
* @param(in) strA first string
* @param(in) strB second string
* @returns 0, if one of these strings is NULL or both are equal
* when they have different lengths: if all chars untils the n-th
* (see description) are equal and strA is shorter 1000000 is returned,
* if the n chars are equal and strB is shorter -1000000 is returned
*/
int comparec( char *strA, char *strB )
{
  /* return value */
  int retval = 0;
  /* loop index and lengths for strings */
  int i = 0, lenA = -1, lenB = -1;
  /* is one string NULL? */
  if ( ( strA == 0 ) || ( strB == 0 ) )
  {
    return retval;
  }
  /* get lengths */
  lenA = lengthc( strA );
  lenB = lengthc( strB );
  /* to the shortest length from strings */
  for ( i = 0; i <= min( lenA, lenB ); i++ )
  {
    /* equal? */
    retval = (int)( strB[i] - strA[i] );
    if ( retval != 0 )
    {
      break;
    }
  }
  /* equal to the n-th char, but different lengths */
  if ( ( retval == 0 ) && ( lenA != lenB ) )
  {
    if ( lenA < lenB )
    {
      retval = 1000000;
    }
    else
    {
      retval = -1000000;
    }
  }
  return retval;
}

/**
* Brief Copies a string to another string, but inserts a
* backslash before every character c.
* @param(in) in string to parse
* @param(in|out) out returned string
* @param(in) c the character to search for
* @returns -1, if an error occured or in or out==NULL,
* 0 otherwise 
*/
int escapeChar( const char *in, char *out, char c )
{
  int retval = 0, i = 0, o = 0, size = 0;
  char lastChar = 0;
  char mbuf[STR_LEN];
  /* delete buffer */
  memset( mbuf, 0, STR_LEN );
  /* check for invalid strings */
  if ( ( in == 0 ) || ( out == 0 ) ) return -1;
  /* get size of in */
  size = length( in ) + 1;
  /* parse in for  */
  for ( i = 0; i < size; i++ )
  {
    /* ..c but not, when there had been a backslas direct in front */
    if ( ( in[i] == c ) && ( lastChar != '\\' ) )
    {
      /* insert backslash */
      mbuf[o++] = '\\';
      retval++;
    }
    /* insert char */
    mbuf[o++] = in[i];
    lastChar = in[i];
  }
  /* end out */
  mbuf[o] = '\0';
  sprintf( out, "%s", mbuf );
  return retval;
}
/**
* Brief gets position of c in string c from position from.
* @param(in) str string to check
* @param(in) c char to search
* @param(in) from earliest position of c
* @returns -1 if not found, else position in str, where c was found
*/
int indexOf( const char *str, char c, int from )
{
  int retval = -1, i = 0;
  /* check string and from for correct settings */
  if ( str == 0 ) return retval;
  if ( from < 0 ) return retval;
  if ( length( str ) <= from ) return retval;
  /* find c */
  for ( i = from; i <= length( str ); i++ )
  {
    if ( str[i] == c )
    {
      retval = i;
      break;
    }
  }
  return retval;
}
/**
* Brief Find the index of the last position of a char in a string,
* but after from
* @param(in) str String to search character in
* @param(in) c character to search for
* @param(in) from earliest position of c
* @returns last position of a c or -1 if invalid or not found
*/
int lastIndexOf( const char *str, char c, int from )
{
  int retval = -1, i = 0;
  /* invalid? return */
  if ( str == 0 ) return retval;
  if ( from < 0 ) return retval;
  if ( length( str ) <= from ) return retval;
  /* search c */
  for ( i = from; i <= length( str ); i++ )
  {
    if ( str[i] == c )
    {
      retval = i;
    }
  }
  return retval;
}
/**
* Brief computes the length of a String.
* Searches for \0 and gives back the number of chars until this one
* @param(in) str string to get the length from
* @returns length of str without \0
*/
int length( const char *str )
{
  int retval = -1;
  /* invalid? */
  if ( str == 0 )
  {
    return retval;
  }
  retval = 0;
  /* search */
  while ( str[retval++] != '\0' ) ;
  /* -2: one because retval was increment, and the other */
  /* because the \0 isn't counted */
  return retval - 2;
}

/**
* Brief computes the length of a String.
* Searches for \0 and gives back the number of chars until this one
* @param(in) str string to get the length from
* @returns length of str
* deprecated use length(char*) instead
*/
int lengthc( char *str )
{
  return length(str);
}

/**
* Brief Copies a part of a string to another string.
* Copies a part of a given string to the beginning of an other string
* start indicates the beginning of the section to copy, end the ending
* @param(in) str String to copy a segment from
* @param(in/out) buf String to write the substring to
* @param(in) start the beginning index of the substring in str
* @param(in) en indicates the index of the last char of the substring in str
* @returns -1 if an error occured, 0 if it could be done
*/
int substring( const char *str, char *buf, int start, int end )
{
  int retval = 1, i = 0, pos = 0, size = 0;
  /* buffer for substring */
  char mbuf[STR_LEN];
  /* setted to 0000... */
  memset( mbuf, 0, STR_LEN );
  /* str is NULL */
  if ( str == 0 )
  {
    return -1;
  }
  /* add 1 for \0 */
  size = length( str ) + 1;
  /* return \0 if invalid end and start */
  if ( ( end - start ) > size )
  {
    retval = 0;
    mbuf[0] = '\0';
  }
  else
  {
    /* copy char by char */
    for ( i = start, pos = 0;
      ( ( i < end ) && ( pos < end - start ) );
      i++, pos++ )
    {
      mbuf[pos] = str[i];
    }
  }
  /* set ending flag */
  mbuf[end - start] = '\0';
  /* copy mbuf to buf */
  sprintf( buf, "%s", mbuf );
  return retval;
}
/**
* Brief Remove beginning and ending whitespaces.
* removes all ' 's and '\t's in beginning and ending of a string and
* returns the result as another string
* @param(in) in incoming string to trim
* @param(in/out) out outgoing, trimmed string
* returns -1, if an error occured, else if trimming was necessary 1, if not 0
*/
int trim( const char *in, char *out )
{
  int retval = 0, i = 0, start = 0, ende = 0, o = 0;
  char mbuf[STR_LEN];
  /* empty buffer */
  memset( mbuf, 0, STR_LEN );
  /* invalid? */
  if ( ( in == 0 ) || ( out == 0 ) ) return -1;
  /* find blanks in the begiining */
  while ( in[start] == ' ' || in[start] == '\t' )
  {
    start++;
    retval = 1;
    if ( start > length( in ) ) break;
  }
  ende = length( in );
  /* find blanks at the end */
  while ( in[ende] == ' ' || in[ende] == '\t' )
  {
    ende--;
    retval = 1;
    if ( ende < 0 ) break;
  }
  /* copy remaining chars */
  for ( i = start; i < ende + 1; i++ )
  {
    mbuf[o++] = in[i];
  }
  /* and append a \0 */
  mbuf[o] = '\0';
  /* copy buffre to out */
  sprintf( out, "%s", mbuf );
  return retval;
}
/**
* Brief removes a character from beginning and ending of a string.
* This function removes a character from the beginning and ending of
* a string. so the string ""aaSomethingaa"" can be trimmed to 
* "Something". A string like "aaaSomethinga" will be trimmed to
* "aaSomething". So you need to have the same number of chars in beginning
* and end of the string. Do you still need this function? Then:
* H4VE FUN!
* @param(in) in string to be trimmed
* @prama(in/out) out the trimmed string
* @param(in) c the character to search for
* @returns 0, if no trimming was done,1 if it was done, -1 for errors
*/
int trimChar( const char *in, char *out, char c )
{
  int retval = 0, i = 0, start = 0, ende = 0, o = 0;
  char mbuf[STR_LEN];
  /* empty buffer */
  memset( mbuf, 0, STR_LEN );
  /* invalid strings */
  if ( ( in == 0 ) || ( out == 0 ) ) return -1;
  /* find end */
  ende = length( in );
  /* check if c is at beginning and end */
  if ( ( in[start] == c ) && ( in[ende] == c ) )
  {
    start++;
    ende--;
    retval = 1;
  }
  /* copy remaining parts */
  for ( i = start; i < ende + 1; i++ )
  {
    mbuf[o++] = in[i];
  }
  /* set \0 to end string */
  mbuf[o] = '\0';
  /* copy buf to out */
  sprintf( out, "%s", mbuf );
  return retval;
}
/**
* Brief Computes the minimum of 2 ints.
* @param(in) a a number
* @param(in) b another number
* @returns the smaller of these numbers
*/
int min( int a, int b )
{
  /* compare a and b (!not bitwise!) */
  if ( a < b )
  /* open bracket for large mass of code */
  /* what should be done if a is SMALLER */
  /* then b, E.g a=1,b=1000! */
  {
    /* if it is smaller return a, NOT b */
    return a;
  }
  /* don't understand these lines, */
  /* but they should be correct */
  else
  {
    return b;
  }
}
/*****************************************************************************
Log-History

$Log: stringlib.c,v $
Revision 1.4  2007/01/12 10:44:35  molka
replaced //-comments and tabs

Revision 1.3  2006/09/29 14:50:12  rschoene
commented

Revision 1.2  2006/01/03 13:45:54  mickler
- Fixed trim function which started at second char

Revision 1.1.1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

Revision 1.2  2005/07/04 12:10:39  wloch
inserted CVS comments and prepended bi to hashtable function names

*****************************************************************************/

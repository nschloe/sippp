/*!@file JBI.c
 * C-implementation of the native methods in ${BENCHITROOT}/jbi/JBI.java
 */

#include "JBI.h"
#include <sys/time.h>
#include <time.h>
/*!
 * @return The elapsed time since 01.01.1970 in seconds.
 */
JNIEXPORT jdouble JNICALL Java_JBI_get_1time_1of_1day (JNIEnv *env, jobject obj)
{

	struct timeval time;
	struct timezone tz;
	
	if (gettimeofday(&time,&tz)==0) return (double) time.tv_sec + (double) time.tv_usec * 1.0e-6;

	return (double) 0; // in case of an error
}


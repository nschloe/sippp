#include <stdio.h>

int main()
{
/* This should work on all compilers except Intel  */
#ifdef __VERSION__
  /* which needs those */
  #ifdef __INTEL_COMPILER
    #ifdef __INTEL_COMPILER_BUILD_DATE
      printf("%i, build %i\n",__INTEL_COMPILER,__INTEL_COMPILER_BUILD_DATE);
    #else
      printf("%s\n",__INTEL_COMPILER);
    #endif
  #else
    printf("%s\n",__VERSION__);
  #endif
#endif
}

#include <iostream>

using namespace std;

int main()
{
/* This should work on all compilers except Intel  */
#ifdef __VERSION__
  /* which needs those */
  #ifdef __INTEL_COMPILER
    #ifdef __INTEL_COMPILER_BUILD_DATE
      cout << __INTEL_COMPILER << ", build "<<__INTEL_COMPILER_BUILD_DATE << endl;
    #else
      cout << __INTEL_COMPILER << endl;
    #endif
  #else
     cout << __VERSION__ << endl;
  #endif
#endif
}

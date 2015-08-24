/*prototypes for matmul1_funcs.c*/
double gettimeroverhead( void );
void deallocate( fds *pmem );
void allocateANDtouch( fds *pmem, int *pisize );
void entry_( void *ptr, int *pisize, int iunrolled );
double getseqentryoverhead( void *pmem );

extern void tstcas_ ( int *in, int *im, int *iunrolled, double *pda, double *pdb, double *pdc );

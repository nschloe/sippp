C
C BenchIT - Performance Measurement for Scientific Applications
C
C Kerneldescription
C Author: Name Lastname (me@myadress.ee)
C
C $Revision: 1.1 $
C $Date: 2007/06/05 18:07:11 $
C $State: Exp $
C
	SUBROUTINE fortranfunction(n)
	INTEGER*4 n
	do 100 i=1,n
	   WRITE (*,*) "Hello world!"
 100	CONTINUE
	RETURN
	END


C
C	LOG-History
C
C	$Log: simple.f,v $
C	Revision 1.1  2007/06/05 18:07:11  william
C	This Skeleton shows how to compile Fortran Codeagainst the BenchIT-Interface written in CFor more information regarding Fortran have a lookat our wiki:http://www.benchit.org/wiki/index.php/Fortran
C
C

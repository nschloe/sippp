/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.4 $
* $Date: 2005/07/21 19:42:13 $
* $State: Exp $
*
***********************************************************************/
import java.util.HashMap;
import java.util.Iterator;
import java.io.*;

public class BIEnvHash
{
   private static BIEnvHash instance = null;
   private HashMap table = null;
   private BIEnvHash()
   {
      bi_initTable();
      bi_fillTable();
   }
   public static BIEnvHash getInstance()
   {
      if ( instance == null ) instance = new BIEnvHash();
      return instance;
   }
   /** Puts a new key value mapping into the map and returns the
     * previously assigned value to the key, or null, if it's a
     * new mapping. */
   public String bi_setEnv( String key, String value )
   {
      return bi_put( key, value );
   }
   /** Puts a new key value mapping into the map and returns the
     * previously assigned value to the key, or null, if it's a
     * new mapping. */
   public String bi_put( String key, String value )
   {
      if ( table == null ) bi_initTable();
      Object ret = table.put( key, value );
      if ( ret == null ) return null;
      return (String)ret;
   }
   public String bi_getEnv( String key )
   {
      return bi_get( key );
   }
   public String bi_get( String key )
   {
      if ( table == null ) return null;
      return (String)(table.get( key ));
   }
   public void bi_dumpTable()
   {
      if ( table == null ) return;
      printf( "\nHashtable dump of all known environment variables at compiletime:" );
      printf( "\n Key            | Value" );
      printf( "\n----------------------------------" );
      Iterator it = table.keySet().iterator();
      while ( it.hasNext() )
      {
         String key = (String)(it.next());
         String value = (String)(table.get( key ));
         printf( "\n " + key + " | " + value );
      }
      printf( "\n" + bi_size() + " entries in total.\n" );
      flush();
   }
   public void dumpToOutputBuffer( StringBuffer outputBuffer )
   {
      outputBuffer.append( "beginofenvironmentvariables\n" );
      Iterator it = table.keySet().iterator();
      while ( it.hasNext() )
      {
         String key = (String)(it.next());
         String value = (String)(table.get( key ));
         StringBuffer b = new StringBuffer();
         for ( int i = 0; i < value.length(); i++ )
         {
            if ( value.charAt( i ) == '"' ) {
               /* check equal number of escape chars->insert escape char */
               int cnt = 0;
               for ( int j = i - 1;  j >= 0; j-- ) {
                  if ( value.charAt( j ) == '\\' ) cnt++;
               }
               /* escape quote */
               if ( cnt % 2 == 0 ) {
                  b.append( '\\' );
               }
            }
            b.append( value.charAt( i ) );
         }
         value = b.toString();
         outputBuffer.append( key + "=\"" + value + "\"\n" );
      }
      outputBuffer.append( "endofenvironmentvariables\n" );
   }
   public int bi_size()
   {
      if ( table != null ) return table.size();
      return -1;
   }
   public void bi_initTable()
   {
      table = new HashMap( 1009 );
   }
   private void printf( String str )
   {
      System.out.print( str );
   }
   private void flush()
   {
      System.out.flush();
      System.err.flush();
   }
   /** Takes a PARAMETER file as argument and adds it's variables
     * to the environment HashMap. */
   public boolean bi_readParameterFile( String fileName )
   {
      boolean retval = false;
      File textFile = null;
      try {
         textFile = new File( fileName );
      }
      catch ( NullPointerException npe ) {
         return retval;
      }
      if ( textFile != null ) {
         if ( textFile.exists() ) {
            FileReader in;
            char ch = '\0';
            int size = (int)(textFile.length());
            int read = 0;
            char inData[] = new char[size + 1];
            try {
               in = new FileReader( textFile );
               in.read( inData, 0, size );
               in.close();
            }
            catch( IOException ex ) {
               return retval;
            } // scan inData for environment variables
            int scanned = 0;
            int it = 0;
            ch = inData[scanned];
            StringBuffer buffer = null;
            while ( scanned < size ) {
               buffer = new StringBuffer();
               // read a line
               while ( ch != '\n' ) {
                  if ( ch != '\r' ) {
                     buffer.append( ch );
                  }
                  scanned += 1;
                  ch = inData[scanned];
               }
               // trim leading and trailing white spaces
               String lineValue = buffer.toString().trim();
               // now check the line
               int eqIndex = -1;
               try {
                  eqIndex = lineValue.indexOf( "=" );
               }
               catch ( NullPointerException npe ) {
                  return retval;
               }
               // only check lines containing an equals sign, no $ sign,
               // and the start with a capital letter
               if ( eqIndex >= 0 && lineValue.indexOf( "$" ) < 0 &&
                  lineValue.substring( 0, 1 ).matches( "[A-Z]" ) ) {
                  String varname = null;
                  String varvalue = null;
                  try {
                     varname = lineValue.substring( 0 , eqIndex );
                     varvalue = lineValue.substring( eqIndex + 1 );
                  }
                  catch ( IndexOutOfBoundsException ioobe ) {
                  }
                  if ( ( varname != null ) && ( varvalue != null ) ) {
                     boolean st1=false, en1=false, st2=false, en2=false;
                     boolean st3=false, en3=false;
                     try {
                        st1=varvalue.startsWith( "'" );
                        en1=varvalue.endsWith( "'" );
                        st2=varvalue.startsWith( "(" );
                        en2=varvalue.endsWith( ")" );
                        st3=varvalue.startsWith( "\"" );
                        en3=varvalue.endsWith( "\"" );
                     }
                     catch ( NullPointerException npe ) {
                        return retval;
                     }
                     if ( ( ( st1 == en1 ) && ( ( st1 != st2 ) && ( st1!=st3 ) ) ) ||
                          ( ( st2 == en2 ) && ( ( st2 != st1 ) && ( st2!=st3 ) ) ) ||
                          ( ( st3 == en3 ) && ( ( st3 != st1 ) && ( st3!=st2 ) ) ) ||
                          ( ( st1 == en1 ) && ( st2 == en2 ) && ( st3 == en3 )
                          && ( st1 == st2 ) && ( st2 == st3 ) && ( st3 == false ) ) ) {
                        table.put( varname, varvalue );
                     }
                  }
               }
               scanned += 1;
               ch = inData[scanned];
               it++;
            }
         }
         else {
            System.err.println( "BenchIT: PARAMETER file \"" + fileName
               + "\" doesn't exist." );
            return retval;
         }
      }
      else {
         System.err.println( "BenchIT: PARAMETER file \"" + fileName
               + "\" not found." );
         return retval;
      }
      return true;
   }
   /* special case: generated from outside and code will be
      appended to this file */
   /** Fills the table with predefined content. */
   public void bi_fillTable()
   {
/*****************************************************************************
Log-History

$Log: BIEnvHash.template.java,v $
Revision 1.4  2005/07/21 19:42:13  wloch
debugged parameter file parsing

Revision 1.3  2005/07/21 13:32:04  wloch
implemented hashtable dump into bit file

Revision 1.2  2005/07/19 12:17:59  wloch
added cvs footer

Revision 1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree


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
}

package system;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

public class BIGKernel implements Comparable {

   private static final boolean debug=false;
   //private String name;
   private String relativePath;
   private File absolutePath;
   private ArrayList subDirs;
   private Map scripts;
   /** the names of the executables */
   private String[] compiledNames=null;

   private int sorting=0;
   private String type;
   private String name;
   private String language;
   private String parallelLibraries;
   private String libraries;
   private String dataType;

   public BIGKernel(String a,Map m)throws StringIndexOutOfBoundsException
   {
      this(new File(a),m);
   }

   public BIGKernel(File a/*, ArrayList s*/, Map m ) throws StringIndexOutOfBoundsException{
      super();
      if (debug)System.err.println(a);
      String n=a.getAbsolutePath();
      n = n.substring(
          BIGInterface.getInstance().getBenchItPath().length() ,
          a.getAbsolutePath().length() ) ;
      if (debug)System.err.println("n="+n);
      if ( n.startsWith( File.separator ) )
      { // s is a separator ;)
         n = n.substring( "skernels".length() ) ;
      }
      else
      {
         n = n.substring( "kernels".length() ) ;
      }
      this.relativePath = n ;
      if (debug)System.err.println("relative path:"+this.relativePath);
      this.type = n.substring( 0 , n.indexOf( File.separator ) ) ;
      if (debug)System.err.println("type "+type);
      this.name = n.substring(n.indexOf(File.separator)+1);
      this.language=this.name.substring(this.name.indexOf(File.separator)+1);
      this.parallelLibraries=this.language.substring(this.language.indexOf(File.separator)+1);
      this.libraries=this.parallelLibraries.substring(this.parallelLibraries.indexOf(File.separator)+1);
      this.dataType=this.libraries.substring(this.libraries.indexOf(File.separator)+1);
      if (debug)System.err.println("name "+name);
      if (debug)System.err.println("language "+language);
      if (debug)System.err.println("parallelLibraries "+parallelLibraries);
      if (debug)System.err.println("libraries "+libraries);
      if (debug)System.err.println("dataType "+dataType);
      this.name=this.name.substring(0,this.name.indexOf(File.separator));
      this.language=this.language.substring(0,this.language.indexOf(File.separator));
      this.parallelLibraries=this.parallelLibraries.substring(0,this.parallelLibraries.indexOf(File.separator));
      this.libraries=this.libraries.substring(0,this.libraries.indexOf(File.separator));
      if (debug)System.err.println("name "+name);
      if (debug)System.err.println("language "+language);
      if (debug)System.err.println("parallelLibraries "+parallelLibraries);
      if (debug)System.err.println("libraries "+libraries);
      if (debug)System.err.println("dataType "+dataType);
      //name = n;
      //relativePath = r;
      absolutePath = a;
//      subDirs = s;
      scripts = m;
//      scripts=BIGExecute.checkShellScriptExistance(getAbsolutePath());
      // String binName=relativePath.replaceAll(File.separator,".");

/*      if ((new File(this.absolutePath+File.separator+"RUN.SH")).exists())
      {
         String[] newComNames=new String[compiledNames.length+1];
         for ( int i = 0 ; i < compiledNames.length ; i++ )
         {
            newComNames[ i ] = compiledNames[ i ] ;
         }
         newComNames[ compiledNames.length ] = ".." + File.separator +
             "kernel" + File.separator + relativePath+File.separator+"RUN.SH" ;
         compiledNames=newComNames;
      }*/
this.reloadExecutables();
   }

   public int compareTo( Object o ) {
      return name.compareTo( o.toString() );
   }
   public int getNumberOfCompilations()
   {
      return this.compiledNames.length;
   }
   public void reloadExecutables()
   {
      final String finBinName=this.getNameAfterSorting(0);
      // checking for bin-path
      File binPath = new File( system.BIGInterface.getInstance().getBenchItPath() +
                               File.separator + "bin" ) ;
      if (!binPath.exists())
      {
         this.compiledNames=new String[0];
      }
      // get the number of executables
      // within directory bin,
      // which start with binName
      this.compiledNames = binPath.list( new java.io.FilenameFilter()
      {
         public boolean accept( File path , String name )
         {
            if ( name.startsWith( finBinName ) )
                  return true ;
            return false ;
         }

      } ) ;
      // setting this.compiledNames[]
/*      for (int i=0;i<compiledNames.length;i++)
      {
         System.err.println(compiledNames[i]);
         if ((new File(binPath+File.separator+compiledNames[i])).isDirectory())
         {
            compiledNames[i]=compiledNames[i]+File.separator+"RUN.SH";
         }
         else
         {
            compiledNames[i]="RUN.SH "+compiledNames[i];
         }
      }*/

   }
   public String[] getCompiledNames()
   {
      return this.compiledNames;
   }
   public String[] askWhichKernel(java.awt.Component parent)
   {
      JList selectionList = new JList( this.compiledNames ) ;
      selectionList.setBorder( BorderFactory.createTitledBorder(
          "available compilations" ) ) ;
      Object[] o = new Object[ 4 ] ;
      o[ 0 ] = "More then one compiled version of the kernel\n" +
          this.name + "\n" +
          "was found. Please select the version(s)\n"
          + "you want to start:" ;
      o[ 1 ] = selectionList ;
      o[ 2 ] = "Please be aware that the last entry is\n"+
         "just a link to the last compiled version" ;
     JTextField jt=new JTextField();
      jt.setBorder(BorderFactory.createTitledBorder("regular expression (includes *'s and ?'s)"));
      o[3]=jt;


      int value = JOptionPane.showConfirmDialog( parent , o , "select compilations" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return null ;
      else
      {
         String[] returnStrings = null;
         if ((jt.getText()!=null) &&(!jt.getText().equals("")))
         {
            Vector vOfEexecs=new Vector();
            // getting matches in JList for regex
            for (int i=0;i<this.compiledNames.length;i++)
            {
               if (gui.BIGRemoteMenu.isRegEx(jt.getText(),this.compiledNames[i]))
               {
                  vOfEexecs.add( this.compiledNames[ i ] ) ;
               }
            }
            // turn Objects into Strings
            o=vOfEexecs.toArray();
            returnStrings=new String[o.length];
            for (int i=0;i<o.length;i++)
            {
               returnStrings[ i ] = ( String ) o[ i ] ;
            }
         } else
         {
            returnStrings =new String[selectionList.getSelectedValues().length];
            for ( int i = 0 ; i < returnStrings.length ; i++ )
               returnStrings[ i ] = ( String ) ( selectionList.
                                                 getSelectedValues()[ i ] ) ;
            if ( returnStrings.length == 0 )return null ;
         }
         return returnStrings ;

      }
   }

   public String getType()
   {
      return this.type;
   }
   public String getLibraries()
   {
      return this.libraries;
   }
   public String getParallelLibraries()
   {
      return this.parallelLibraries;
   }
   public String getSourceLanguage()
   {
      return this.language;
   }
   public String getDataType()
   {
      return this.dataType;
   }

   public String[] getSortedNames(int sorting)
   {
      String[] sortedNames= new String[6 ] ;
      // initial setting
      sortedNames[0]=type;
      sortedNames[1]=name;
      sortedNames[2]=language;
      sortedNames[3]=parallelLibraries;
      sortedNames[4]=libraries;
      sortedNames[5]=dataType;
      String tempString=sortedNames[sorting];
      for (int i=sorting;i>0;i--)
         sortedNames[i]=sortedNames[i-1];
      sortedNames[0]=tempString;
      return sortedNames;
   }
   public String getNameAfterSorting(int sorting)
   {
      String[] names=this.getSortedNames(sorting);
      String name=new String();
      for (int i=0;i<names.length;i++)
      {
         name=name+"."+names[i];
      }
      if (debug)System.err.println(name.substring(1));
      return name.substring(1);
   }

   public File[] getFiles()
   {
      File[] files= this.absolutePath.listFiles(new FileFilter()
          {
             public boolean accept(File f)
             {
                if (f.isDirectory())
                   return false;
                return isFilenameWanted(f.getName());
             }
          });
      //for (int i=0;i<files.length;i++)
      //   System.err.println(files[i]);
      return files;
   }
   /**
    * checks whether to display the file or not
    * @param name String
    * @return boolean
    */
   public static boolean isFilenameWanted(String name)
   {
      String deprFiles=null;
      try
      {
         deprFiles = BIGInterface.getInstance().getBIGConfigFileParser().
             stringCheckOut( "DontShowFileList" ) ;
      }
      catch ( Exception ex )
      {
        if (debug)System.err.println("no list");
         //no list
         return true;
      }
      StringTokenizer st=new StringTokenizer(deprFiles,";");
      while (st.hasMoreTokens())
      {
         String token=st.nextToken();
         if ( gui.BIGRemoteMenu.isRegEx( token.trim() , name ) )
            return false ;
      }
      return true;
   }

   public String getName()
   {
      return name ;
   }

   public String getRelativePath()
   {
      return relativePath ;
   }

   public String getAbsolutePath()
   {
      return absolutePath.getAbsolutePath() ;
   }

   public ArrayList getSubDirList()
   {
      return subDirs ;
   }

   public Map getScripts()
   {
      return scripts ;
   }

   public String toString()
   {
      return getSortedNames(this.sorting)[5] ;
   }
   public void setSorting(int s)
   {
      this.sorting=s;
   }
}

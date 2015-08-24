package system;

/**
 * <p>Überschrift: BenchIT</p>
 * <p>Beschreibung: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR TU Dresden</p>
 * @author Robert Schoene
 * @version 1.0
 */
import java.io.* ;

public class BIGFileHelper
{
   /**
    * copies a file (or folder) to a given folder
    * THE NEW FILES WILL NOT HAVE THE EXECUTABLE BIT!!!
    * @param fileToCopy File the source File or folder
    * @param destinationFolder File the destination File or Folder
    * @param overwrite boolean whether to overwrite existing Files or not
    */
   public static void copyToFolder( File fileToCopy , File destinationFolder,boolean overwrite )
   {
      destinationFolder.mkdirs();
      while (!destinationFolder.exists());
      if ( fileToCopy.isFile() )
      {
         File newFile = new File( destinationFolder.getAbsolutePath() +
                                  File.separator + fileToCopy.getName() ) ;
         if ( newFile.exists() )
         {
            if ( !overwrite )
                  // not allowed to overwrite
                  return ;
         }
         FileInputStream fis = null ;
         try
         {
            fis = new FileInputStream( fileToCopy ) ;
         }
         catch ( FileNotFoundException ex )
         {
            System.err.println( "File " + fileToCopy.getAbsolutePath() + " not found." ) ;
            return ;
         }
         FileOutputStream fos = null ;
         try
         {
            fos = new FileOutputStream( newFile ) ;
         }
         catch ( FileNotFoundException ex1 )
         {
           System.err.println( "File " + newFile.getAbsolutePath() + " not found." ) ;
            return ;
         }
         byte[] buffer = new byte[ 1000 ] ;
         int readData = 0 ;
         do
         {
            try
            {
               readData = fis.read( buffer ) ;
               if ( readData > 0 )
                   fos.write( buffer , 0 , readData ) ;
            }
            catch ( IOException ex2 )
            {
                System.err.println( "Couldn't copy File" ) ;
            }
         }
//         while ( readData == 1000 ) ;
         while ( readData > 0 ) ;
         try
         {
            fis.close() ;
            fos.close() ;
         }
         catch ( IOException ioe )
         {
            System.err.println( "Couldn't close FileStreams" ) ;
         }
         // if it is a folder
      }
      else
      {
         File newFolder = new File( destinationFolder.getAbsolutePath() +
                                    File.separator +
                                    fileToCopy.getName() ) ;
         // make directory
         if ( ! ( newFolder ).mkdirs() )
         {
            //System.err.println( "Couldn't create directory " + newFolder ) ;
         }
        while(!newFolder.exists());
         // copy files in directory
         File[] fileInFolder = fileToCopy.listFiles() ;
         for ( int i = 0 ; i < fileInFolder.length ; i++ )
         {
            BIGFileHelper.copyToFolder( fileInFolder[ i ] , newFolder,overwrite ) ;
         }
      }
   }
   /**
    * returns content of a File as String
    * @param f File
    * @return String
    */
   public static String getFileContent(File f)
   {
      if (f.isDirectory())
      {
         System.err.println(f.getName()+" is a directory.");
         return null;
      }
      StringBuffer fileContent = new StringBuffer() ;
      byte[] buffer=new byte[1000];
      FileInputStream fis = null ;
      try
      {
         fis = new FileInputStream( f ) ;
      }
      catch ( FileNotFoundException ex )
      {
         System.err.println( "File " + f.getName() + " not found." ) ;
         return null;
      }
      int readData = 0 ;
      do
      {
         try
         {
            readData = fis.read( buffer ) ;
         }
         catch ( IOException ex2 )
         {
            System.err.println( "Couldn't read File" ) ;
         }
         if (readData>0)
            fileContent.append(new String(buffer,0,readData));
      }
      while ( readData == 1000 ) ;
      try
      {
         fis.close() ;
      }
      catch ( IOException ioe )
      {
         System.err.println( "Couldn't close FileStream" ) ;
      }
      String s=null;
      try
      {
      s = new String(new byte[0],"ASCII") ;
      } catch (UnsupportedEncodingException uee)
      	{
		System.err.println("Warning: ASCII-Encoding not supported, String may not work correctly when resaved.");
		return fileContent.toString();
	}
      s=s+fileContent.toString();
      return s;
   }
   /**
    * removes a File or directory
    * @param f File the file or directory to remove
    */
   public static void remove(File f)
   {
      if (f.isFile())
      {
         f.delete();
      }
      else
      {
          File[] sub=f.listFiles();
          for (int i=0;i<sub.length;i++)
          {
             BIGFileHelper.remove(sub[i]);
          }
          f.delete();
      }
   }
   public static void saveToFile(String s,File f)
   {
      try
      {
         FileOutputStream fos = new FileOutputStream( f ) ;
         fos.write( s.getBytes() ) ;
         fos.close() ;
      }
      catch ( FileNotFoundException ex )
      {
         System.err.println("Couldn't find File");
      }
      catch ( IOException ex )
      {
         System.err.println("Error while writing File");
      }
   }
   /**
    * gets all sub files (including f) for file f, writes them to subfiles
    * @param f File
    * @param subFiles File[]
    * @param index int
    */
   public static File[] getAllSubFiles(File f, File[] subFiles, int index)
   {
      if (subFiles==null)
      {
         subFiles = new File[getNumberOfSubFiles( f ) ] ;
         //System.err.println("Init subFiles (length="+subFiles.length+")");
         index=0;
      }
      if (f.isFile())
      {
         //System.err.println("File "+f.getName()+" is a file, writing it to "+index);
         subFiles[index]=f;
         return subFiles;
      }
      if (f.isDirectory())
      {
         //System.err.println("File "+f.getName()+" is a directory, writing it to "+index);
         subFiles[index]=f;
         index++;
         File[] ownSubFiles=f.listFiles();
         for ( int i = 0 ; i < ownSubFiles.length ; i++ )
         {
         //System.err.println("Calculating subFile "+ownSubFiles[i].getName()+", beginning at "+index);
         subFiles = getAllSubFiles( ownSubFiles[ i ] , subFiles , index ) ;
         /*System.err.println( " subFile " + ownSubFiles[ i ].getName() +
                             " has " + getNumberOfSubFiles( ownSubFiles[ i ] ) +
                             ", next one will begin at " + index +"+" +
                             getNumberOfSubFiles( ownSubFiles[ i ] ) ) ;*/
            index=index+getNumberOfSubFiles(ownSubFiles[i]);
         }

      }
      return subFiles;
   }
   /**
    * gets the number of subfiles (if f.isFile()->1),(if !f.exists()->0);
    * @param f File
    * @return int
    */
   public static int getNumberOfSubFiles(File f)
   {
      if (!f.exists()) return 0;
      if (f.isFile()) return 1;
      if (f.isDirectory())
      {
         int numberOfFiles=1;
         File[] subFiles=f.listFiles();
         for ( int i = 0 ; i < subFiles.length ; i++ )
         {
            numberOfFiles=numberOfFiles+getNumberOfSubFiles(subFiles[i]);
         }
         return numberOfFiles;
      }
      return 0;
   }

}

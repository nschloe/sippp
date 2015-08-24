package system ;

import java.io.File;

/**
 * <p>Überschrift: BenchIT</p>
 *
 * <p>Beschreibung: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organisation: ZHR TU Dresden</p>
 *
 * @author Robert Schoene
 * @version 1.0
 */

public class RemoteDefinition {
   //ip or dns-name of remote machine
    private String ip;
    // username on remotemachine
    private String username;
    // name of folder on remote machine
    private String folderName;
    // selected in dialog
    private boolean isSelected;
    // use scp (please dont)
    private boolean useSCP;
    // password is asked once per session and stored
    private String password=null;
    //hostname of the system
    private String hostname;
    // use specific localdef?
    private boolean useLocaldef=false;
    // name of the localdef
    private String localdef=null;
    // bash --login or sh or whatever
    private String shellCmd=null;

    public RemoteDefinition(String ip, String username, String folderName,
                            boolean isSelected,boolean useSCP, String hostname,
                            boolean useLocaldef,String localdef,String shellCmd)
    {
        this.ip = ip;
        this.useSCP=useSCP;
        this.username = username;
        this.folderName = folderName;
        this.isSelected = isSelected;
        this.hostname=hostname;
        this.useLocaldef=useLocaldef;
        this.localdef=localdef;
        this.shellCmd=shellCmd;
    }
    public String toString() {
        return username + "@" + ip + ":~/" + folderName + ((this.getLOCALDEF()!=null) ? "("+this.getLOCALDEF()+")" : "");
    }

    public String getUsername() {
        return username;
    }

    public String getIP() {
        return this.ip;
    }

    public String getFoldername() {
        return this.folderName;
    }

    public void setSelected(boolean b) {
        this.isSelected = b;
    }

    public boolean isSelected() {
        return this.isSelected;
    }

    public String getHostname() {
        return this.hostname;
     }
     public void setHostname(String hostname) {
        this.hostname=hostname;
     }

    public String getXML() {
        StringBuffer sb = new StringBuffer();
        sb.append("<RemoteDefinition>\n");
        sb.append("   <ip> ");
        sb.append(this.ip);
        sb.append(" </ip>\n");
        sb.append("   <username> ");
        sb.append(this.username);
        sb.append(" </username>\n");
        sb.append("   <foldername> ");
        sb.append(this.folderName);
        sb.append(" </foldername>\n");
        sb.append("   <isSelected> ");
        sb.append(this.isSelected);
        sb.append(" </isSelected>\n");
        sb.append("   <hostname> ");
        sb.append(this.hostname);
        sb.append(" </hostname>\n");
        sb.append("   <useSCP> ");
        sb.append(this.useSCP);
        sb.append(" </useSCP>\n");
        sb.append("   <useLOCALDEF> ");
        sb.append(this.useLocaldef);
        sb.append(" </useLOCALDEF>\n");
        sb.append("   <LOCALDEF> ");
        sb.append(this.localdef);
        sb.append(" </LOCALDEF>\n");
        if (this.shellCmd!=null)
        {
          sb.append("   <SHELLCMD> ");
          sb.append(this.shellCmd);
          sb.append(" </SHELLCMD>\n");
        }
        sb.append("</RemoteDefinition>\n");

        return sb.toString();
    }

    public static String getXMLStarting() {
        return "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n" +
                "<!DOCTYPE RemoteDefinition [\n" +
                "   <!ELEMENT RemoteDefinition ( ip,username,foldername,isSelected,hostname ) >\n" +
                "   <!ELEMENT ip         (#PCDATA)>\n" +
                "   <!ELEMENT username   (#PCDATA)>\n" +
                "   <!ELEMENT foldername (#PCDATA)>\n" +
                "   <!ELEMENT isSelected (#PCDATA)>\n" +
                "   <!ELEMENT useSCP (#PCDATA)>\n" +
                "   <!ELEMENT useLOCALDEF (#PCDATA)>\n" +
                "   <!ELEMENT LOCALDEF (#PCDATA)>\n" +
                "   <!ELEMENT SHELLCMD (#PCDATA)>\n" +
                "]>\n\r";
    }

    public static RemoteDefinition[] getRemoteDefsFromXML(File f) {
        // read File to String
        if (!((f.exists()) && (f.isFile()))) {
            return new RemoteDefinition[0];
        }
        String filecontent = BIGFileHelper.getFileContent(f);
        java.util.ArrayList al = new java.util.ArrayList();
        while (filecontent.indexOf("<RemoteDefinition>") > -1) {
            String ip = filecontent.substring(filecontent.indexOf("<ip> ") + 5,
                                              filecontent.indexOf(" </ip>"));
            String username = filecontent.substring(filecontent.indexOf(
                    "<username> ") + 11, filecontent.indexOf(" </username>"));
            String foldername = filecontent.substring(filecontent.indexOf(
                    "<foldername> ") + 13, filecontent.indexOf(" </foldername>"));
            boolean isSelected = false;

            try {
                if (filecontent.substring(filecontent.indexOf(
                        "<isSelected> ") + 13,
                                          filecontent.indexOf(" </isSelected>")).
                    equals("true")) {
                    isSelected = true;
                }
            } catch (Exception ignored) {
             }
             String hostname=null;
            boolean useSCP=false;

            try {
                if (filecontent.substring(filecontent.indexOf(
                        "<useSCP> ") + 9,
                                          filecontent.indexOf(" </useSCP>")).
                    equals("true")) {
                    useSCP = true;
                }
            } catch (Exception ignored) {
             }
             boolean useLocaldef=false;
            try {
                if (filecontent.substring(filecontent.indexOf(
                        "<useLOCALDEF> ") + 14,
                                          filecontent.indexOf(" </useLOCALDEF>")).
                    equals("true")) {
                    useLocaldef = true;
                }
            } catch (Exception ignored) {
             }

             String localdef=null;
            try {
               localdef=filecontent.substring(filecontent.indexOf(
                        "<LOCALDEF> ") + 11,
                                          filecontent.indexOf(" </LOCALDEF>"));
            } catch (Exception ignored) {
             }


             String shellCmd=null;
            try {
               shellCmd=filecontent.substring(filecontent.indexOf(
                        "<SHELLCMD> ") + 11,
                                          filecontent.indexOf(" </SHELLCMD>"));
            } catch (Exception ignored) {
             }

            al.add(new RemoteDefinition(ip, username, foldername, isSelected,useSCP,
                                        hostname,useLocaldef,localdef,shellCmd));
            filecontent = filecontent.substring(filecontent.indexOf(
                    "</RemoteDefinition>\n") + 19, filecontent.length());
        }
        RemoteDefinition[] ret = new RemoteDefinition[al.size()];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = (RemoteDefinition) al.get(i);
        }
        return ret;
    }

    public static void saveRemoteDefsToXML(RemoteDefinition[] defs, File f) {
        StringBuffer sb = new StringBuffer();
        sb.append(RemoteDefinition.getXMLStarting());
        for (int i = 0; i < defs.length; i++)
            sb.append(defs[i].getXML());
        BIGFileHelper.saveToFile(sb.toString(), f);
    }

    public String getPassword()
    {
        return this.password;
    }
    public void setPassword(String p)
    {
        this.password=p;
    }
    public boolean getUseSCP()
    {
       return this.useSCP;
    }
    public String getLOCALDEF()
    {
       if (!this.useLocaldef)
          return this.getHostname();
       if ((this.localdef==null)||(this.localdef.equals("null")))
          return this.getHostname();
       return this.localdef;
    }
    public String getShellCommand()
    {
       String command="bash --login";
      try
      {
         command = BIGInterface.getInstance().getBIGConfigFileParser().
             stringCheckOut( "remoteShellCmd" ) ;
      }
      catch ( Exception ex )
      {
         System.out.println("remoteShellCmd not set in BGUI.cfg. Using default \"bash --login\"");
         System.out.println("To run BenchIT on a system without bash, close the GUI, open the file");
         System.out.println("BGUI.cfg and set the flag \"remoteShellCmd=sh\"");
      }

       if (this.shellCmd==null)
          return command;
       if (this.shellCmd.equals(""))
          return command;
       return shellCmd;

    }


}

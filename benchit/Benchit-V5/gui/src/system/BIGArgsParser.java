/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGArgsParser.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.3 $
*  $Date: 2005/12/07 10:25:24 $
*
******************************************************************************/
package system;

//import java.util.*;

/**
 * The parser for the args given on program starts.
 * <P>
 * valid arguments are:
 * <BR><code>
 * -config=[filename] ..... for changing the name of the XML configfile<BR>
 * -host=[hostname] ....... for changing the name of the actually host<BR>
 * -admin ................. for starting the adminTool instead of the normal GUI<BR>
 * -debug[class]=[level] .. for changing the debug level of a class<BR>
 * -debug ................. for setting globally debuglevel<BR>
 * -win ................... for setting windows look and feel (needn't to funktion well)<BR>
 * -enableAdminNew......... enables the "New"-Button in Adminmode
 * -restart................ supresses the daily tip after measurement shutdown an shows result tree after restart<BR>
 * <BR>
 * [filename] .. just the name of a file, maybe with path<BR>
 * [hostname] .. a word what is the name of the host<BR>
 * [class] ..... the name of a class in the BIG project<BR>
 * [level] ..... the debug level, 0 is off, 1 is on<BR>
 * </code><BR>
 * Examples:
 * <BR><code>
 * -config=config.xml<BR>
 * -host=remus<BR>
 * -debugBIGDefFileParser=1<BR>
 * -debugBIGXMLParser=on<BR>
 * -debug<BR>
 * -debug=5<BR>
 * </code><BR>
 * Of course there can be more then one argument and the order doesn't play any
 * role.
 *
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGInterface
 **/
public class BIGArgsParser {

	private boolean debug = false;

	private BIGInterface db;

	/**
	 * Constructs a BIGArgsParser.
	 **/
	public BIGArgsParser() {
		db = BIGInterface.getInstance();
		if (db.getDebug("BIGArgsParser") > 0)
			debug = true;
	}

	/**
	 * Parses the given arguments into the <code>Interface</code>.
	 *
	 * @param   args        the given arguments
	 **/
        public void parse(String[] args) throws BIGParserException {
            BIGStrings errorText = new BIGStrings();
		int pos = 0;
		while (pos < args.length) {
			String arg;
			if (args[pos] != null) {
				arg = new String(args[pos]);
			} else {
				arg = new String();
			}
            if ((arg.startsWith("\"")) && (arg.endsWith("\"")))
                arg = arg.substring(2,arg.length()-2);
			if (debug) System.out.println("BIGArgsParser: line " + pos + " org = " + arg);

			// it is a debug level
			if (arg.startsWith("-debug")) {
				arg = arg.substring(6).replaceAll("\"", "");
				String what;
				String level;
				if (arg.equals("")) {
					// all on debug
					db.setDebug(1);
				} else {
					if (arg.indexOf("=") >= 0) {
						what = arg.substring(0, arg.indexOf("="));
						level = arg.substring(arg.indexOf("=") + 1);
					} else {
						what = arg;
						level = "1";
					}
					what = what.replaceAll(" ", "");
					level = level.replaceAll(" ", "").toLowerCase();
					if ((level.equals("on"))
						|| (level.equals("yes"))
						|| (level.equals("true"))) {
						level = "1";
					}
					if ((level.equals("off"))
						|| (level.equals("no"))
						|| (level.equals("false"))) {
						level = "0";
					}
					if (debug)
						System.out.println(
							"BIGArgsParser: what: " + what + " level: " + level);
					try {
						db.setDebug(what, Integer.valueOf(level).intValue());
					} catch (Exception e) {
						errorText.add(
							"pos "
								+ pos
								+ ": ["
								+ args[pos]
								+ "] is not a valid argument");
					}
				}
				arg = "";
			}

			// set another configFilename
			if (arg.startsWith("-config")) {
				arg = arg.substring(7);
				String filename;
				if (arg.indexOf("=") >= 0) {
					filename = arg.substring(arg.indexOf("=") + 1);
					while (filename.startsWith(" "))
						filename = filename.substring(1);
					while (filename.endsWith(" "))
						filename = filename.substring(0, filename.length() - 1);
					db.setConfigFilename(filename);
					if (debug)
						System.out.println(
							"BIGArgsParser: configFilename: [" + filename + "]");
				} else {
					if (arg.replaceAll(" ", "").equals("")) {
						errorText.add(
							"pos " + pos + ": forgotten config filename?");
					} else {
						errorText.add(
							"pos "
								+ pos
								+ ": ["
								+ args[pos]
								+ "] is not a valid argument");
					}
				}
				arg = "";
			}

			// set another hostname
			if (arg.startsWith("-host")) {
				arg = arg.substring(5).replaceAll("\"", "");
				String hostname;
				if (arg.indexOf("=") >= 0) {
					hostname = arg.substring(arg.indexOf("=") + 1);
					while (hostname.startsWith(" "))
						hostname = hostname.substring(1);
					while (hostname.endsWith(" "))
						hostname = hostname.substring(0, hostname.length() - 1);
                                             db.setOriginalHost(hostname);
					db.setHost(hostname);
					if (debug)
						System.out.println("BIGArgsParser: hostname: [" + hostname + "]");
				} else {
					if (arg.replaceAll(" ", "").equals("")) {
						errorText.add("pos " + pos + ": forgotten hostname?");
					} else {
						errorText.add(
							"pos "
								+ pos
								+ ": ["
								+ args[pos]
								+ "] is not a valid argument");
					}
				}
				arg = "";
			}

			// set windows look and feel
			if (arg.startsWith("-win")) {
				db.setLookAndFeel("win");
				arg = "";
			}

			// load admintool or not
/*			if (arg.startsWith("-admin")) {
				db.setLoadAdminTool(true);
				arg = "";
			}*/

         // allow admintool to create new entries or not
/*         if (arg.startsWith("-enableAdminNew")) {
               db.setShowAdminNew(true);
               arg = "";
         }*/

         // set restart flag
         if (arg.startsWith("-restart")) {
            db.setRestart( true );
            arg = "";
         }

         pos++;
      }
      if (errorText.size() > 0) {
         throw new BIGParserException("BIGArgsParser: \n" + errorText);
      }
   }

}
/*****************************************************************************
Log-History

*****************************************************************************/

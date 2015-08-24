/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  part of the BenchIT-project (java version)                         *
*
*  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*  Last change by: $Author: wloch $
*  $Revision: 1.1.1.1 $
*  $Date: 2005/07/18 13:03:19 $
*
******************************************************************************/
import java.util.GregorianCalendar;
import java.util.StringTokenizer;
import java.util.HashSet;

/** This class provides the functionality of formating
  * the time provided through GregorianCalendar at instanciation
  * time in several ways. It is able to return the day of the
  * week and month as a String. */
public class RWDate extends GregorianCalendar
{
   private String delim = " .,:;/-_\0\n\r\t\f";
   private HashSet delimiterSet = new HashSet();
   private HashSet argumentsSet = new HashSet();
   /** The constructor. */
   public RWDate()
   {
      super();
      delimiterSet.add(" ");
      delimiterSet.add(".");
      delimiterSet.add(",");
      delimiterSet.add(":");
      delimiterSet.add(";");
      delimiterSet.add("/");
      delimiterSet.add("-");
      delimiterSet.add("_");
      delimiterSet.add("\0");
      delimiterSet.add("\n");
      delimiterSet.add("\r");
      delimiterSet.add("\t");
      delimiterSet.add("\f");
      argumentsSet.add("%dd");
      argumentsSet.add("%d");
      argumentsSet.add("%DD");
      argumentsSet.add("%D");
      argumentsSet.add("%mm");
      argumentsSet.add("%m");
      argumentsSet.add("%MM");
      argumentsSet.add("%M");
      argumentsSet.add("%y");
      argumentsSet.add("%0y");
      argumentsSet.add("%yy");
      argumentsSet.add("%Y");
      argumentsSet.add("#HH");
      argumentsSet.add("#H");
      argumentsSet.add("#hh");
      argumentsSet.add("#h");
      argumentsSet.add("#mm");
      argumentsSet.add("#m");
      argumentsSet.add("#ss");
      argumentsSet.add("#s");
   }
   /** Returns a String representing the current time. The
     * formatString argument must stick to a few rules, however:
     * Each date field must start with the % sign. Allowed are only
     * the letters: d (day), m (month), y (year), h (hour),
     * m (minute), s (seconds). For day and month capital letters
     * result in the spelled out name, e.g. %D: "Mon". You can use
     * double letters to force long output, e.g. %DD: "Monday", or
     * %dd: "01" instead of %d: "1". For the year you can choose
     * between 4 digit version (%yy) or the smaller version. However,
     * using %y can result in a one digit year number. But by
     * using %0y you'll definitely get a two digit year number.<br>
     * Each time field must start with the # sign. If you use
     * double letters in time fields occationally leading zeros
     * will be inserted.
     * For hour capital letters result in 24 hour format.<br>
     * Examples are:<br>
     * "%D, %MM %d., %Y": "Mon, January 3., 2004", or
     * "%dd.%mm.%y, #HH:#mm:#ss": "03.01.04, 15:33:09", or
     * "%DD, the %dd.%mm.%Y": "Monday, the 03.01.2004". */
   public String formatDate(String formatString)
   {
      String retVal = "error";
      boolean returnDelimiters = true;
      boolean invalidArgument = false;
      boolean longName = true;
      StringBuffer outBuffer = null;
      StringTokenizer st = new StringTokenizer(formatString, delim, returnDelimiters);
      while (st.hasMoreTokens() && !invalidArgument)
      {
         if (outBuffer == null && !invalidArgument) outBuffer = new StringBuffer();
         String token = st.nextToken();
         if (delimiterSet.contains(token) && !invalidArgument) outBuffer.append(token);
         else if (argumentsSet.contains(token) && !invalidArgument)
         {
            if (token.equals("%dd"))
            {
               if (get(DAY_OF_MONTH) < 10) outBuffer.append("0");
               outBuffer.append(get(DAY_OF_MONTH));
            }
            else if (token.equals("%d"))
            {
               outBuffer.append(get(DAY_OF_MONTH));
            }
            else if (token.equals("%DD"))
            {
               outBuffer.append(getDayOfWeek(longName));
            }
            else if (token.equals("%D"))
            {
               outBuffer.append(getDayOfWeek(!longName));
            }
            else if (token.equals("%mm"))
            {
               if (get(MONTH) < 9) outBuffer.append("0");
               outBuffer.append(get(MONTH) + 1);
            }
            else if (token.equals("%m"))
            {
               outBuffer.append(get(MONTH) + 1);
            }
            else if (token.equals("%MM"))
            {
               outBuffer.append(getMonth(longName));
            }
            else if (token.equals("%M"))
            {
               outBuffer.append(getMonth(!longName));
            }
            else if (token.equals("%y"))
            {
               int y = get(YEAR);
               StringBuffer year = new StringBuffer();
               year.append(y);
               year = year.delete(0, year.length() - 2);
               if (year.charAt(0) == '0') year = year.deleteCharAt(0);
               outBuffer.append(year.toString());
            }
            else if (token.equals("%0y"))
            {
               int y = get(YEAR);
               StringBuffer year = new StringBuffer();
               year.append(y);
               year = year.delete(0, year.length() - 2);
               outBuffer.append(year.toString());
            }
            else if (token.equals("%yy"))
            {
               int y = get(YEAR);
               StringBuffer year = new StringBuffer();
               year.append(y);
               outBuffer.append(year.toString());
            }
            else if (token.equals("%Y"))
            {
               outBuffer.append(get(YEAR));
            }
            else if (token.equals("#HH"))
            {
               int h = get(HOUR);
               if (get(AM_PM) == PM) h += 12;
               if (h < 10) outBuffer.append("0");
               outBuffer.append(h);
            }
            else if (token.equals("#H"))
            {
               int h = get(HOUR);
               if (get(AM_PM) == PM) h += 12;
               outBuffer.append(h);
            }
            else if (token.equals("#hh"))
            {
               int h = get(HOUR);
               if (get(AM_PM) == PM) outBuffer.append("pm ");
               if (get(AM_PM) == AM) outBuffer.append("am ");
               if (h < 10) outBuffer.append("0");
               outBuffer.append(h);
            }
            else if (token.equals("#h"))
            {
               if (get(AM_PM) == PM) outBuffer.append("pm ");
               if (get(AM_PM) == AM) outBuffer.append("am ");
               outBuffer.append(get(HOUR));
            }
            else if (token.equals("#mm"))
            {
               int m = get(MINUTE);
               if (m < 10) outBuffer.append("0");
               outBuffer.append(m);
            }
            else if (token.equals("#m"))
            {
               int m = get(MINUTE);
               outBuffer.append(m);
            }
            else if (token.equals("#ss"))
            {
               int s = get(SECOND);
               if (s < 10) outBuffer.append("0");
               outBuffer.append(s);
            }
            else if (token.equals("#s"))
            {
               int s = get(SECOND);
               outBuffer.append(s);
            }
         }
         else if (token.startsWith("%") || token.startsWith("#"))
         {
            invalidArgument = true;
            retVal = "invalid Argument: " + token;
            outBuffer = null;
         }
         else outBuffer.append(token);
      }
      if (outBuffer != null && !invalidArgument) retVal = outBuffer.toString();
      return retVal;
   }
   /** Returns a name for the first day of the week.
     * This can either be Monday or Sunday. */
   public String getFirstDayOfWeek(boolean longName)
   {
      String retVal = "error";
      switch (getFirstDayOfWeek())
      {
         case SUNDAY : if (longName) retVal = "Sunday"; else retVal = "Sun"; break;
         case MONDAY : if (longName) retVal = "Monday"; else retVal = "Mon"; break;
      }
      return retVal;
   }
   /** Returns the current day of the week as a String. */
   public String getDayOfWeek(boolean longName)
   {
      String retVal = "error";
      switch (get(DAY_OF_WEEK))
      {
         case MONDAY : if (longName) retVal = "Monday"; else retVal = "Mon"; break;
         case TUESDAY : if (longName) retVal = "Tuesday"; else retVal = "Tue"; break;
         case WEDNESDAY : if (longName) retVal = "Wednesday"; else retVal = "Wed"; break;
         case THURSDAY : if (longName) retVal = "Thursday"; else retVal = "Thu"; break;
         case FRIDAY : if (longName) retVal = "Friday"; else retVal = "Fri"; break;
         case SATURDAY : if (longName) retVal = "Saturday"; else retVal = "Sat"; break;
         case SUNDAY : if (longName) retVal = "Sunday"; else retVal = "Sun"; break;
      }
      return retVal;
   }
   /** Returns the current month as a String. */
   public String getMonth(boolean longName)
   {
      String retVal = "error";
      switch (get(MONTH))
      {
         case JANUARY : if (longName) retVal = "January"; else retVal = "Jan"; break;
         case FEBRUARY : if (longName) retVal = "February"; else retVal = "Feb"; break;
         case MARCH : if (longName) retVal = "March"; else retVal = "Mar"; break;
         case APRIL : if (longName) retVal = "April"; else retVal = "Apr"; break;
         case MAY : if (longName) retVal = "May"; else retVal = "May"; break;
         case JUNE : if (longName) retVal = "June"; else retVal = "Jun"; break;
         case JULY : if (longName) retVal = "July"; else retVal = "Jul"; break;
         case AUGUST : if (longName) retVal = "August"; else retVal = "Aug"; break;
         case SEPTEMBER : if (longName) retVal = "September"; else retVal = "Sep"; break;
         case OCTOBER : if (longName) retVal = "October"; else retVal = "Oct"; break;
         case NOVEMBER : if (longName) retVal = "November"; else retVal = "Nov"; break;
         case DECEMBER : if (longName) retVal = "December"; else retVal = "Dec"; break;
      }
      return retVal;
   }
}

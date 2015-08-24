/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGEntry.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.4 $
*  $Date: 2007/01/15 06:41:24 $
*
******************************************************************************/
package system;

import java.awt.Component;
import java.io.FileFilter;
import java.io.File;

/**
 * Basic entry the interface uses in its datebase. This represents one
 * parameter of the DEF-files, with its value and other options to set
 * for a graphical view, plus some options only uses in runtime.
 * <P>
 * here is a list of all options can be set.
 * <code>
 * <BR>private int pos;
 * <BR>private String name;
 * <BR>private int type;
 * <BR>private boolean activeStatus;
 * <BR>private String viewName;
 * <BR>private String toolTipText;
 * <BR>private String help;
 * <BR>private Object action;
 * <BR>private int priority;
 * <BR>private boolean necessity;
 * <BR>private Object value;
 * <BR>private Object defaultValue;
 * <BR>private Object prototypeValue;
 * <BR>private BIGStrings multipleChoice;
 * <BR>private int multipleChoiceCount;
 * <BR>private Component guiRepresentation, guiRepresentationLabel;
 * </code>
 *
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGInterface
 **/

public class BIGEntry implements Comparable {

    private int pos;
    private String name;
    private int type;
    private boolean activeStatus;
    private String viewName;
    private String toolTipText;
    private String help;
    private Object action;
    private int priority;
    private boolean necessity;
    private Object value;
    private Object defaultValue;
    private Object prototypeValue;
    private BIGStrings multipleChoice;
    private int multipleChoiceCount;
    private Component guiRepresentation, guiRepresentationLabel;

    /**
     * Constructs an empty entry, with the given name.
     * The name is necessary.
     *
     * @param   name    the name of the entry.
     **/
    public BIGEntry(String name) {
        resetToDefault();
        this.name = name;
    }

    /**
     * Constructs an empty entry, with the given name and type.
     * The name is necessary.
     *
     * @param   name    the name of the entry.
     * @param   type    the type of the entry.
     **/
    public BIGEntry(String name, int type) {
        resetToDefault();
        this.name = name;
        this.type = type;
    }

    /**
     * Resets the whole entry back to default.
     *
     **/
    private void resetToDefault() {
        pos = 0;
//        name =
        type = 0;
        activeStatus = true;
        viewName = "default name";
        toolTipText = null;
        help = null;
        action = null;
        priority = 0;
        necessity = true;
        defaultValue = null;
        value = null;
        multipleChoice = null;
        multipleChoiceCount = 1;

    }

    /**
     * Returns the position on whitch this entry is shown in graphic-mode.
     *
     * @return          the position on whitch this entry is shown
     **/
    public int getPos() {
        return pos;
    }

    /**
     * Sets the position on whitch this entry is shown in graphic-mode.
     *
     * @param   pos     the position on whitch this entry is shown
     **/
    public void setPos(int pos) {
        this.pos = pos;
    }

    /**
     * Returns the name of the entry, like it is called in the def-files.
     *
     * @return          the name of the entry
     **/
    public String getName() {
        return name;
    }

    /**
     * Returns the type of the entry.
     * <BR>possible types are: coming soon
     *
     * @return          the type of the entry
     **/
    public int getType() {
        return type;
    }

    /**
     * Sets the type of the entry.
     * This will only work one time, if the type isn't already set.<BR>
     * Or you are in the admin mode.
     * <BR>possible types are:<BR>
     * NONE .. this is when no type is set<BR>
     * INTEGER .. a number<BR>
     * FLOAT .. a number (in java this is <code>Double</code>)<BR>
     * BOOLEAN .. true/false 1/0<BR>
     * MULTIPLE .. one sting of a set<BR>
     * STRING .. something like "bla"<BR>
     * LIST .. list of strings of given set<BR>
     * DATETIME .. this is date and time<BR>
     * VECTOR .. a set of strings<BR>
     *
     * @param   type            the type of the entry
     **/
    public void setType(int type) throws BIGAccessViolationException {
        if ((this.type == 0) /*|| (BIGInterface.getInstance().getLoadAdminTool())*/) {
            if ((type == BIGInterface.NONE) ||
                (type == BIGInterface.INTEGER) ||
                (type == BIGInterface.FLOAT) ||
                (type == BIGInterface.BOOLEAN) ||
                (type == BIGInterface.MULTIPLE) ||
                (type == BIGInterface.STRING) ||
                (type == BIGInterface.LIST) ||
                (type == BIGInterface.DATETIME) ||
                (type == BIGInterface.VECTOR) ||
                (type == BIGInterface.FREE_TEXT)) {
                this.type = type;
            } else {
                throw new BIGAccessViolationException("BIGEntry: tried to set an invalid type [" + type + "] on " + name);
            }
        } else {
            throw new BIGAccessViolationException("BIGEntry: type already set (org: " + this.type + ",new: " + type + ") on " + name);
        }
    }

    /**
     * Returns the activeStatus of the entry, whether the entry is shown or
     * something like deleted.
     * <BR>true  = active
     * <BR>false = "deleted"
     *
     * @return          the activeStatus of the entry
     **/
    public boolean getActiveStatus() {
        return activeStatus;
    }

    /**
     * Sets the activeStatus of the entry, whether the entry is shown or
     * something like deleted.
     * <BR>true  = active
     * <BR>false = "deleted"
     *
     * @param   activeStatus    the activeStatus of the entry
     **/
    public void setActiveStatus(boolean activeStatus) {
        this.activeStatus = activeStatus;
    }

    /**
     * Returns the name of the entry, like it is shown in the graphic-mode.
     *
     * @return          the viewName of the entry
     **/
    public String getViewName() {
        return viewName;
    }

    /**
     * Sets the name of the entry, like it is shown in the graphic-mode.
     *
     * @param   viewName        the viewName of the entry
     **/
    public void setViewName(String viewName) {
        this.viewName = viewName;
    }

    /**
     * Returns the toolTipText of the entry, the text what is shown if
     * you go over this entry with your mouse in the graphic-mode.
     *
     * @return          the toolTipText of the entry
     **/
    public String getToolTipText() {
        return toolTipText;
    }

    /**
     * Sets the toolTipText of the entry, the text what is shown if
     * you go over this entry with your mouse in the graphic-mode.
     *
     * @param   toolTipText     the toolTipText of the entry
     **/
    public void setToolTipText(String toolTipText) {
        this.toolTipText = toolTipText;
    }

    /**
     * Returns the help of the entry, the text should help to
     * understand what this entry is for.
     *
     * @return          the help of the entry
     **/
    public String getHelp() {
        return help;
    }

    /**
     * Sets the help of the entry, the text should help to
     * understand what this entry is for.
     *
     * @param   help            the help of the entry
     **/
    public void setHelp(String help) {
        this.help = help;
    }

    /**
     * Returns the action of the entry, what happens when
     * the value of this entry is changed.
     * <BR><b>not yet implemented</b>
     *
     * @return          the action of the entry
     **/
    public Object getAction() {
        return action;
    }

    /**
     * Sets the action of the entry, what happens when
     * the value of this entry is changed.
     * <BR><b>not yet implemented</b>
     *
     * @param   action          the action of the entry
     **/
    public void setAction(Object action) {
        this.action = action;
    }

    /**
     * Returns the priority of the entry, on which
     * priority level the entry is shown.
     *
     * @return          the priority of the entry
     **/
    public int getPriority() {
        return priority;
    }

    /**
     * Sets the priority of the entry, on which
     * priority level the entry is shown.
     * And also increases the maximumViewLevel in the
     * Interface if nessessary.
     *
     * @param   priority        the priority of the entry
     **/
    public void setPriority(int priority) {
        BIGInterface.getInstance().setMaximumViewLevel(priority);
        this.priority = priority;
    }

    /**
     * Returns the necessity of the entry, whether the
     * the entry is importent (written bold) or it isn't
     * (written normal).
     *
     * @return          the necessity of the entry
     **/
    public boolean getNecessity() {
        return necessity;
    }

    /**
     * Sets the necessity of the entry, whether the
     * the entry is importent (written bold) or it isn't
     * (written normal).
     *
     * @param   necessity     the necessity of the entry
     **/
    public void setNecessity(boolean necessity) {
        this.necessity = necessity;
    }

    /**
     * Returns the value of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @return          the value of the entry
     **/
    public Object getValue() {
        return value;
    }

    /**
     * Sets the value of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @param   value           the value of the entry
     **/
    public void setValue(Object value) throws BIGAccessViolationException {
        this.value = verifyValue(value);
    }

    /**
     * Returns the defaultValue of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @return          the defaultValue of the entry
     **/
    public Object getDefaultValue() {
        return defaultValue;
    }

    /**
     * Sets the defaultValue of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @param   defaultValue    the defaultValue of the entry
     **/
    public void setDefaultValue(Object defaultValue) throws BIGAccessViolationException {
        this.defaultValue = verifyValue(defaultValue);
    }

    /**
     * Returns the PrototypeValue of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @return          the value of the entry
     **/
    public Object getPrototypeValue() {
        return prototypeValue;
    }

    /**
     * Sets the PrototypeValue of the entry.
     * The type is <code>Object</code> because it can be one of
     * <code>Integer</code>, <code>Boolean</code>, <code>String</code>
     * or <code>BIGStrings</code>.
     *
     * @param   prototypeValue           the value of the entry
     **/
    public void setPrototypeValue(Object prototypeValue) throws BIGAccessViolationException {
        this.prototypeValue = verifyValue(prototypeValue);
    }


    /**
     * Returns the multipleChoice of the entry, the set of possible
     * selections.
     *
     * @return          the multipleChoice of the entry
     **/
    public BIGStrings getMultipleChoice() {
        return multipleChoice;
    }

    /**
     * Sets the multipleChoice of the entry, the set of possible
     * selections.
     *
     * @param   multipleChoice  the multipleChoice of the entry
     **/
    public void setMultipleChoice(BIGStrings multipleChoice) throws BIGAccessViolationException {
        if (multipleChoice == null) {
            this.multipleChoice = multipleChoice;
        } else {
            try {
                if (type == 0) {
                    throw new BIGAccessViolationException("BIGEntry: type of " + name + " isn't set jet");
                }
                if ((type == BIGInterface.INTEGER) ||
                    (type == BIGInterface.FLOAT) ||
                    (type == BIGInterface.BOOLEAN) ||
                    (type == BIGInterface.STRING) ||
                    (type == BIGInterface.DATETIME) ||
                    (type == BIGInterface.VECTOR) ||
                    (type == BIGInterface.FREE_TEXT)) {
                    throw new BIGAccessViolationException("BIGEntry: this type (" + type + ") of " + name + " hasn't a multiple choice");
                }
                if (type == BIGInterface.MULTIPLE) {
                    this.multipleChoice = multipleChoice;
                }
                if (type == BIGInterface.LIST) {
                    this.multipleChoice = multipleChoice;
                }
            }
            catch (Exception e) {
                throw new BIGAccessViolationException("BIGEntry: the value of " + name + " hasn't the right type: " + value + " isn't " + type);
            }
        }
    }

    /**
     * Returns the multipleChoiceCount of the entry, the max number
     * of selections what can be choosen simultaneously.
     *
     * @return          the multipleChoiceCount of the entry
     **/
    public int getMultipleChoiceCount() {
        return multipleChoiceCount;
    }

    /**
     * Sets the multipleChoiceCount of the entry, the max number
     * of selections what can be choosen simultaneously.
     *
     * @param   multipleChoiceCount     the multipleChoiceCount of the entry
     **/
    public void setMultipleChoiceCount(int multipleChoiceCount) {
        this.multipleChoiceCount = multipleChoiceCount;
    }

    /**
     * Sets the representation of the entry in the GUI in form of a
     * <code>java.awt.Component</code>.
     *
     * @param   c       the component that is the gui representation
     **/
    public void setComponent(Component c) {
        this.guiRepresentation = c;
    }

    /**
     * Returns the representation label of the entry in the GUI in form of a
     * <code>java.awt.Component</code>.
     *
     * @return          the component that is the gui representation label
     **/
    public Component getLabel() {
        return guiRepresentationLabel;
    }

    /**
     * Sets the representation label of the entry in the GUI in form of a
     * <code>java.awt.Component</code>.
     *
     * @param   c       the component that is the gui representation label
     **/
    public void setLabel(Component c) {
        this.guiRepresentationLabel = c;
    }

    /**
     * Returns the representation of the entry in the GUI in form of a
     * <code>java.awt.Component</code>.
     *
     * @return          the component that is the gui representation
     **/
    public Component getComponent() {
        return guiRepresentation;
    }

    /**
     * Compares two entries by its positions.
     *
     * @return          a negative integer, zero, or a positive integer
     *                  as the position of the specified entry is greater than,
     *                  equal to, or less than this entrys.
     **/
    public int compareTo(Object o) {
        return pos-((BIGEntry) o).getPos();
    }

    public boolean equals(Object o) {
        return ((BIGEntry)o).getName().equals(getName());
    }

    /**
     * Returns a string representation of the entry.
     * This is the name of the entry.
     *
     * @return          a string representation of the object
     **/
    public String toString() {
        return name;
    }

    private Object verifyValue(Object value) throws BIGAccessViolationException {
        Object result = null;
        if (type == 0) {
            throw new BIGAccessViolationException("BIGEntry: on setValue of " + name + " type is set none");
        }
        try {
            if (type == BIGInterface.INTEGER) {
               if (value!=null)
                result = (Integer) value;
            }
            if (type == BIGInterface.FLOAT) {
                result = (Double) value;
            }
            if (type == BIGInterface.BOOLEAN) {
                result = (Boolean) value;
            }
            if (type == BIGInterface.MULTIPLE) {
                result = (String) value;

                // overriding environments. they are stored in a specific folder
                if (getName().equals("BENCHIT_ENVIRONMENT"))
                {
                   String newValue="";
                   String environmentPath = BIGInterface.getInstance().getBenchItPath() +
                       File.separator + "tools" + File.separator +
                       "environments" ;
                   File[] environmentFiles = ( new File( environmentPath ) ).
                       listFiles( new FileFilter()
                   {
                      public boolean accept( File f )
                      {
                         if ( f.isFile() )
                            return true ;
                         return false ;
                      }
                   } ) ;
                   String[] names=new String[environmentFiles.length];
                   for (int i=0;i<names.length;i++)
                   {
                      names[i]=environmentFiles[i].getName();
                   }
                   multipleChoice=new BIGStrings(names);
                   multipleChoice.sort();
                   multipleChoiceCount=environmentFiles.length;
                }

            }
            if (type == BIGInterface.STRING) {
                result = (String) value;
            }
            if (type == BIGInterface.LIST) {
                result = (BIGStrings) value;
            }
            /*if (type == BIGInterface.DATETIME) {
                result = (Calendar) value;
            }*/
            if (type == BIGInterface.VECTOR) {
                result = (BIGStrings) value;
             }
             if (type == BIGInterface.FREE_TEXT) {
                 result = (String) value;
             }

        }
        catch (Exception e) {
            throw new BIGAccessViolationException("BIGEntry: the value of " + name + " hasn't the right type.");
        }
        return result;
    }

    public boolean verify() {
        boolean result = true;

        try {
            if (type != BIGInterface.NONE) {
                //setDefaultValue(defaultValue);
                setValue(value);
            } else {
                if (value != null) {
                    System.err.println("WARNING: " + name + " is type NONE but has an value.");
                }
                if (defaultValue != null) {
                    System.err.println("WARNING: " + name + " is type NONE but has an defaultValue.");
                }
            }
            if ((type != BIGInterface.MULTIPLE) && (type != BIGInterface.LIST)) {
                if (multipleChoice != null){
                    System.err.println("WARNING: " + name + " is not type MULTIPLE or LIST but has an multipleChoice.");
                }
                if (multipleChoiceCount != 1) {
                    System.err.println("WARNING: " + name + " is not type MULTIPLE or LIST but multipleChoiceCount is unequals 1.");
                }
            }

        }
        catch (Exception e) {
            System.err.println("ERROR: " + name + " is not valid combination of type [" + type + "] and value or defaultValue.");
            result = false;
        }

        return result;
    }
}
/*****************************************************************************
Log-History

*****************************************************************************/

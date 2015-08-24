package conn;

import java.awt.*;
import javax.swing.*;

/**
 *
 * <p>Überschrift: MyJPanel</p>
 * <p>Beschreibung: the main JPanel of the conn-package</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */
public class MyJPanel{


    FunctionList functionList;
    public JTabbedPane panel = new JTabbedPane();
    private SocketCon socketCon;
    conn.GraphData graphData;
    public MainEditPanel lowerRightPanel;
    public ProgressPanel progressPanel = new ProgressPanel();
    private MajorJPanel major;
    private Container lowerLeftPanel;
    /**
     * trys to open a connection to the server and paints itself
     * @param ip the ip of the server
     * @param username the username to login
     * @param password the password of the username
     * @throws java.lang.Exception thrown if login failed
     */
    public MyJPanel(String ip, String username, String password,
                    MajorJPanel major, gui.BIGGUI gui) throws Exception {
        //then we try to open the Connection
        this.setSocketCon(ip, username, password,gui);
        this.major = major;
        this.graphData = new GraphData(this.progressPanel);
        this.graphData.setSocketCon(this.socketCon);
        lowerRightPanel = new MainEditPanel(this.socketCon, this.graphData, this,gui);
        lowerRightPanel.setConsolePanel(system.BIGInterface.getInstance().
                                        getConsole().getDisplayPanel());
        panel = MainViewPanel.update(this.graphData);
    }

    /**
     * sets the socketCon (and opens a connection)
     * @param ip the ip of the server
     * @param username the username to login
     * @param password the password for the user
     * @throws java.lang.Exception is throw if login failed
     */
    protected void setSocketCon(String ip, String username, String password, gui.BIGGUI gui) throws
            Exception {
        this.socketCon = new SocketCon(ip, username, password,
                                       this.progressPanel, this, gui);
    }

    /**
     * sets this socketCon
     * @param s the new socketCon
     */
    protected void setSocketCon(SocketCon s) {
        this.socketCon = s;
    }

    /**
     * gets this socketCon
     * @return this' socketCon
     */
    protected SocketCon getSocketCon() {
        return this.socketCon;
    }

    /**
     * closes the connection
     */
    public void closeSocketCon() {
        major.closeConnection();
    }

    public void setConsole(gui.BIGGUI biggui) {
        lowerLeftPanel = biggui.getBIGInterface().getConsole().getDisplayPanel();
        this.lowerRightPanel.setConsolePanel(lowerLeftPanel);
    }

    public void updateView(final GraphData gd) {
        Thread t = new Thread() {
            public void run() {
                panel = MainViewPanel.update(gd);
            }
        };
        SwingUtilities.invokeLater(t);
    }

}

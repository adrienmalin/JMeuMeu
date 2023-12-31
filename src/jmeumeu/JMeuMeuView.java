/*
 * JMeuMeuView.java
 */

package jmeumeu;

import org.jdesktop.application.Action;
import org.jdesktop.application.SingleFrameApplication;
import org.jdesktop.application.FrameView;
import javax.swing.JDialog;
import javax.swing.JFrame;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * The application's main frame.
 */
public class JMeuMeuView extends FrameView {

    public JMeuMeuView(SingleFrameApplication app) {
        super(app);
        this.getFrame().setTitle("Bienvenue");
        initComponents();
        imagePanel1.SetBackground(this.getFrame(), "./images/accueil.jpg");
        _Oracle = new JMeuMeuOracleAccess();
    }

    @Action
    public void showAboutBox() {
        if (aboutBox == null) {
            JFrame mainFrame = JMeuMeuApp.getApplication().getMainFrame();
            aboutBox = new JMeuMeuAboutBox(mainFrame);
            aboutBox.setLocationRelativeTo(mainFrame);
        }
        
        JMeuMeuApp.getApplication().show(aboutBox);
    }
    
    @Action
    public void showCreateBox(JMeuMeuView View)
    {
        JFrame mainFrame = jmeumeu.JMeuMeuApp.getApplication().getMainFrame();

        if(_CreateUser == null)
        {
            _CreateUser = new jmeumeu.JMeuMeuMenuCreerCompteView(View);  
        }
        
        JMeuMeuApp.getApplication().show(_CreateUser);
        _CreateUser.setSize(430, 290);
        _CreateUser.setLocationRelativeTo(mainFrame);
    }
    
    @Action
    public void showGameField(JMeuMeuView View)
    {
        if(_GameField == null)
        {
            JFrame mainFrame = JMeuMeuApp.getApplication().getMainFrame();
            _GameField = new jmeumeu.JMeuMeuGameField(View);
            _GameField.setLocationRelativeTo(mainFrame);
        }
        
        JMeuMeuApp.getApplication().show(_GameField);
    }
    
    @Action
    public void showSuppressUser(JMeuMeuView View)
    {
        if(_SuppressUser == null)
        {
            JFrame mainFrame = JMeuMeuApp.getApplication().getMainFrame();
            _SuppressUser = new jmeumeu.JMeuMeuDeleteUser(View);
            _SuppressUser.setSize(100, 100);
            _SuppressUser.setLocationRelativeTo(mainFrame);
        }
        
        JMeuMeuApp.getApplication().show(_SuppressUser);
    }
    
    @Action
    public void hide()
    {
        JMeuMeuApp.getApplication().hide(this);
    }
    
    public void show()
    {
        JMeuMeuApp.getApplication().show(this);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        mainPanel = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jTextField1 = new javax.swing.JTextField();
        jPasswordField1 = new javax.swing.JPasswordField();
        imagePanel1 = new Components.ImagePanel();
        jButton4 = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();

        mainPanel.setMaximumSize(new java.awt.Dimension(424, 410));
        mainPanel.setMinimumSize(new java.awt.Dimension(424, 410));
        mainPanel.setName("mainPanel"); // NOI18N
        mainPanel.setPreferredSize(new java.awt.Dimension(424, 410));

        org.jdesktop.application.ResourceMap resourceMap = org.jdesktop.application.Application.getInstance(jmeumeu.JMeuMeuApp.class).getContext().getResourceMap(JMeuMeuView.class);
        jLabel1.setFont(resourceMap.getFont("lblJMeuMeu.font")); // NOI18N
        jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel1.setText(resourceMap.getString("lblJMeuMeu.text")); // NOI18N
        jLabel1.setName("lblJMeuMeu"); // NOI18N

        jTextField1.setText(resourceMap.getString("txtUserName.text")); // NOI18N
        jTextField1.setName("txtUserName"); // NOI18N
        jTextField1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextField1ActionPerformed(evt);
            }
        });

        jPasswordField1.setText(resourceMap.getString("txtPwd.text")); // NOI18N
        jPasswordField1.setName("txtPwd"); // NOI18N

        imagePanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        imagePanel1.setMaximumSize(new java.awt.Dimension(407, 264));
        imagePanel1.setName("imagePanel1"); // NOI18N
        imagePanel1.setPreferredSize(new java.awt.Dimension(407, 264));
        imagePanel1.setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());

        jButton4.setText(resourceMap.getString("jButton4.text")); // NOI18N
        jButton4.setActionCommand(resourceMap.getString("jButton4.actionCommand")); // NOI18N
        jButton4.setName("jButton4"); // NOI18N
        jButton4.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonActionPerformed(evt);
            }
        });

        jLabel2.setFont(resourceMap.getFont("lblUserName.font")); // NOI18N
        jLabel2.setForeground(resourceMap.getColor("lblUserName.foreground")); // NOI18N
        jLabel2.setText(resourceMap.getString("lblUserName.text")); // NOI18N
        jLabel2.setName("lblUserName"); // NOI18N

        jLabel3.setFont(resourceMap.getFont("lblPwd.font")); // NOI18N
        jLabel3.setForeground(resourceMap.getColor("lblPwd.foreground")); // NOI18N
        jLabel3.setText(resourceMap.getString("lblPwd.text")); // NOI18N
        jLabel3.setName("lblPwd"); // NOI18N

        jButton1.setText(resourceMap.getString("btnOK.text")); // NOI18N
        jButton1.setName("btnOK"); // NOI18N
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonActionPerformed(evt);
            }
        });

        jButton3.setText(resourceMap.getString("btnCreate.text")); // NOI18N
        jButton3.setActionCommand(resourceMap.getString("btnCreate.actionCommand")); // NOI18N
        jButton3.setName("btnCreate"); // NOI18N
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonActionPerformed(evt);
            }
        });

        jButton5.setText(resourceMap.getString("jButton5.text")); // NOI18N
        jButton5.setActionCommand(resourceMap.getString("jButton5.actionCommand")); // NOI18N
        jButton5.setName("jButton5"); // NOI18N
        jButton5.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonActionPerformed(evt);
            }
        });

        jButton2.setText(resourceMap.getString("btnAnnuler.text")); // NOI18N
        jButton2.setName("btnAnnuler"); // NOI18N
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ButtonActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout mainPanelLayout = new javax.swing.GroupLayout(mainPanel);
        mainPanel.setLayout(mainPanelLayout);
        mainPanelLayout.setHorizontalGroup(
            mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(mainPanelLayout.createSequentialGroup()
                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(mainPanelLayout.createSequentialGroup()
                        .addGap(48, 48, 48)
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 45, Short.MAX_VALUE)
                        .addComponent(jButton4))
                    .addGroup(mainPanelLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(imagePanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(mainPanelLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addGroup(mainPanelLayout.createSequentialGroup()
                                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel2)
                                    .addComponent(jLabel3))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jPasswordField1)
                                    .addComponent(jTextField1, javax.swing.GroupLayout.DEFAULT_SIZE, 142, Short.MAX_VALUE)))
                            .addComponent(jButton1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButton2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 168, Short.MAX_VALUE)
                            .addComponent(jButton5, javax.swing.GroupLayout.DEFAULT_SIZE, 168, Short.MAX_VALUE)
                            .addComponent(jButton3, javax.swing.GroupLayout.PREFERRED_SIZE, 168, javax.swing.GroupLayout.PREFERRED_SIZE))))
                .addContainerGap())
        );
        mainPanelLayout.setVerticalGroup(
            mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(mainPanelLayout.createSequentialGroup()
                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(mainPanelLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jButton4)))
                    .addGroup(mainPanelLayout.createSequentialGroup()
                        .addGap(46, 46, 46)
                        .addComponent(imagePanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel2)
                            .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButton3))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jButton5)
                    .addComponent(jPasswordField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(mainPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton2)
                    .addComponent(jButton1))
                .addContainerGap(17, Short.MAX_VALUE))
        );

        jButton4.getAccessibleContext().setAccessibleName(resourceMap.getString("jButton4.AccessibleContext.accessibleName")); // NOI18N

        setComponent(mainPanel);
    }// </editor-fold>//GEN-END:initComponents

private void ButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_ButtonActionPerformed

    String action = evt.getActionCommand();
    
    if(action.equals("Quitter"))
    {
        if(JMeuMeuHelpers.ShowQuestionMessage("Voulez-vous quitter le jeu ?") == 0)
            System.exit(0);
    }
    else if(action.equals("CreerCompte"))
    {
        showCreateBox(this);
        hide();
        
    }
    else if(action.equals("Connexion"))
    {      
        if(_Oracle.Connexion())
        {      	
        	String x = this.jTextField1.getText();
        	String y = JMeuMeuHelpers.ToString(this.jPasswordField1.getPassword());
        	
        	
            if(_Oracle.callProc("Connection", x+"','"+y))
            {
            	_Oracle.SetFarmDatas(x);
            	this._Datas = this._Oracle.GetDatas();
                this.hide();
                this.showGameField(this);
            }
        }
    }
    else if(action.equals("About"))
    {
        this.showAboutBox();
    }
    else if(action.equals("SuppCompte"))
    {
        showSuppressUser(this);
        hide();
    }
}//GEN-LAST:event_ButtonActionPerformed

private void jTextField1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jTextField1ActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jTextField1ActionPerformed
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private Components.ImagePanel imagePanel1;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPasswordField jPasswordField1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JPanel mainPanel;
    // End of variables declaration//GEN-END:variables

    private JDialog             aboutBox;
    private JFrame              _CreateUser;
    private JFrame              _SuppressUser;
    private JFrame              _GameField;
    public JMeuMeuDatas         _Datas;
    public JMeuMeuOracleAccess _Oracle;
}

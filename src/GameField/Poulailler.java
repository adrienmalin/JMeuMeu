/*
 * Poulailler.java
 *
 * Created on 7 mai 2008, 17:09
 */

package GameField;

import jmeumeu.JMeuMeuView;

/**
 *
 * @author  Jeremy
 */
public class Poulailler extends javax.swing.JPanel {

    /** Creates new form Poulailler */
    public Poulailler(JMeuMeuView View, jmeumeu.JMeuMeuGameField Field) {
        initComponents();
        _View = View;
        _Field = Field;
        imagePanel1.SetBackground(View.getFrame(), "./images/poulailler.jpg");
        
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        panel1 = new java.awt.Panel();
        imagePanel1 = new Components.ImagePanel();

        setName("Form"); // NOI18N

        org.jdesktop.application.ResourceMap resourceMap = org.jdesktop.application.Application.getInstance(jmeumeu.JMeuMeuApp.class).getContext().getResourceMap(Poulailler.class);
        jLabel1.setFont(resourceMap.getFont("jLabel1.font")); // NOI18N
        jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel1.setText(resourceMap.getString("jLabel1.text")); // NOI18N
        jLabel1.setName("jLabel1"); // NOI18N

        panel1.setBackground(resourceMap.getColor("panel1.background")); // NOI18N
        panel1.setName("panel1"); // NOI18N

        imagePanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        imagePanel1.setName("imagePanel1"); // NOI18N

        javax.swing.GroupLayout imagePanel1Layout = new javax.swing.GroupLayout(imagePanel1);
        imagePanel1.setLayout(imagePanel1Layout);
        imagePanel1Layout.setHorizontalGroup(
            imagePanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 179, Short.MAX_VALUE)
        );
        imagePanel1Layout.setVerticalGroup(
            imagePanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 176, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout panel1Layout = new javax.swing.GroupLayout(panel1);
        panel1.setLayout(panel1Layout);
        panel1Layout.setHorizontalGroup(
            panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panel1Layout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(imagePanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
        panel1Layout.setVerticalGroup(
            panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(imagePanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, 771, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(304, 304, 304)
                        .addComponent(panel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(panel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(261, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

     public void Show()
    {
        setVisible(true);
        ReloadDatas();
    }
    
    private void ReloadDatas()
    {
    	_View._Oracle.SetFarmDatas(_View._Datas.nom);
    	_Field.SetEcus();
        FillDatas();
    }
    
    private void FillDatas()
    {
      	/*this.jLabel8.setText(_View._Datas.nb_malade_vache > 0 ? "Malade" : "Sain");
      	this.jLabel9.setText(_View._Datas.sale_vache.equals("V") ? "Sale" : "Propre");
      	this.jLabel10.setText(Integer.toString(_View._Datas.poids_vache));
      	this.jLabel11.setText(Integer.toString(_View._Datas.nb_lait));
      	this.jLabel12.setText(_View._Datas.abreuvage_vache.equals("F") ? "Non" : "Oui");
      	this.jLabel13.setText(_View._Datas.nourri_vache.equals("F") ? "Non" : "Oui" );
      	this.jLabel15.setText(Integer.toString(_View._Datas.age_vache));
      	
      	if(jLabel8.getText().equals("Sain"))
      		JMeuMeuHelpers.Disabled(jButton3);
      	else
      		JMeuMeuHelpers.Enabled(jButton3);
      	
      	
      	if(jLabel9.getText().equals("Propre"))
      		JMeuMeuHelpers.Disabled(jButton4);
      	else
      		JMeuMeuHelpers.Enabled(jButton4);
      	
      	int i = Integer.parseInt(jLabel11.getText());
      	if(i == 0)
      		JMeuMeuHelpers.Disabled(jButton5);
      	else
      		JMeuMeuHelpers.Enabled(jButton5);
      	
      	if(jLabel12.getText().equals("Oui"))
      		JMeuMeuHelpers.Disabled(jButton1);
      	else
      		JMeuMeuHelpers.Enabled(jButton1);
      	
      	if(jLabel13.getText().equals("Oui"))
      		JMeuMeuHelpers.Disabled(jButton2);
      	else
      		JMeuMeuHelpers.Enabled(jButton2);*/
    }
    
    public void Hide()
    {
        setVisible(false);
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private Components.ImagePanel imagePanel1;
    private javax.swing.JLabel jLabel1;
    private java.awt.Panel panel1;
    // End of variables declaration//GEN-END:variables

    private JMeuMeuView _View;
    private jmeumeu.JMeuMeuGameField _Field;
}

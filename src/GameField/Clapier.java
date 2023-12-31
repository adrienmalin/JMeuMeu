/*
 * Clapier.java
 *
 * Created on 7 mai 2008, 17:15
 */

package GameField;

import jmeumeu.JMeuMeuView;

/**
 *
 * @author  Jeremy
 */
public class Clapier extends javax.swing.JPanel {

    /** Creates new form Clapier */
    public Clapier(JMeuMeuView View, jmeumeu.JMeuMeuGameField Field) {
        initComponents();
        _View = View;
        _Field = Field;
        imagePanelLapereau.SetBackground(View.getFrame(), "./images/lapereau.jpg");
        imagePanelLapin.SetBackground(View.getFrame(), "./images/lapin.jpg");
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        imagePanelLapereau = new Components.ImagePanel();
        jLabel1 = new javax.swing.JLabel();
        imagePanelLapin = new Components.ImagePanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jButtonAbreuverJ = new javax.swing.JButton();
        jButtonNourrirJ = new javax.swing.JButton();
        jButtonSoignerJ = new javax.swing.JButton();
        jButtonNettoyerJ = new javax.swing.JButton();
        jButtonNourrirV = new javax.swing.JButton();
        jButtonAbreuverV = new javax.swing.JButton();
        jButtonNettoyerV = new javax.swing.JButton();
        jButtonSoignerV = new javax.swing.JButton();
        panel1 = new java.awt.Panel();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        jLabel6 = new javax.swing.JLabel();
        jLabelNbMaladesJ = new javax.swing.JLabel();
        jLabelNourrisJ = new javax.swing.JLabel();
        jLabelDesalteresJ = new javax.swing.JLabel();
        panel3 = new java.awt.Panel();
        jLabel12 = new javax.swing.JLabel();
        jLabel13 = new javax.swing.JLabel();
        jLabel14 = new javax.swing.JLabel();
        jLabelNbMaladesL = new javax.swing.JLabel();
        jLabelNourrisL = new javax.swing.JLabel();
        jLabelDesalteresL = new javax.swing.JLabel();

        setName("Form"); // NOI18N

        imagePanelLapereau.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        imagePanelLapereau.setMaximumSize(new java.awt.Dimension(330, 330));
        imagePanelLapereau.setMinimumSize(new java.awt.Dimension(330, 330));
        imagePanelLapereau.setName("imagePanelLapereau"); // NOI18N

        javax.swing.GroupLayout imagePanelLapereauLayout = new javax.swing.GroupLayout(imagePanelLapereau);
        imagePanelLapereau.setLayout(imagePanelLapereauLayout);
        imagePanelLapereauLayout.setHorizontalGroup(
            imagePanelLapereauLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 228, Short.MAX_VALUE)
        );
        imagePanelLapereauLayout.setVerticalGroup(
            imagePanelLapereauLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 203, Short.MAX_VALUE)
        );

        org.jdesktop.application.ResourceMap resourceMap = org.jdesktop.application.Application.getInstance(jmeumeu.JMeuMeuApp.class).getContext().getResourceMap(Clapier.class);
        jLabel1.setFont(resourceMap.getFont("jLabel1.font")); // NOI18N
        jLabel1.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel1.setText(resourceMap.getString("jLabel1.text")); // NOI18N
        jLabel1.setName("jLabel1"); // NOI18N

        imagePanelLapin.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));
        imagePanelLapin.setMaximumSize(new java.awt.Dimension(330, 330));
        imagePanelLapin.setMinimumSize(new java.awt.Dimension(330, 330));
        imagePanelLapin.setName("imagePanelLapin"); // NOI18N

        javax.swing.GroupLayout imagePanelLapinLayout = new javax.swing.GroupLayout(imagePanelLapin);
        imagePanelLapin.setLayout(imagePanelLapinLayout);
        imagePanelLapinLayout.setHorizontalGroup(
            imagePanelLapinLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 228, Short.MAX_VALUE)
        );
        imagePanelLapinLayout.setVerticalGroup(
            imagePanelLapinLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 203, Short.MAX_VALUE)
        );

        jLabel2.setFont(resourceMap.getFont("jLabel2.font")); // NOI18N
        jLabel2.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel2.setText(resourceMap.getString("jLabel2.text")); // NOI18N
        jLabel2.setName("jLabel2"); // NOI18N

        jLabel3.setFont(resourceMap.getFont("jLabel3.font")); // NOI18N
        jLabel3.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabel3.setText(resourceMap.getString("jLabel3.text")); // NOI18N
        jLabel3.setName("jLabel3"); // NOI18N

        jButtonAbreuverJ.setText(resourceMap.getString("jButtonAbreuverJ.text")); // NOI18N
        jButtonAbreuverJ.setName("jButtonAbreuverJ"); // NOI18N
        jButtonAbreuverJ.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonAbreuverJMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonAbreuverJMouseExited(evt);
            }
        });
        jButtonAbreuverJ.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAbreuverJButtonActionPerformed(evt);
            }
        });

        jButtonNourrirJ.setText(resourceMap.getString("jButtonNourrirJ.text")); // NOI18N
        jButtonNourrirJ.setName("jButtonNourrirJ"); // NOI18N
        jButtonNourrirJ.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonNourrirJMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonNourrirJMouseExited(evt);
            }
        });
        jButtonNourrirJ.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonNourrirJButtonActionPerformed(evt);
            }
        });

        jButtonSoignerJ.setText(resourceMap.getString("jButtonSoignerJ.text")); // NOI18N
        jButtonSoignerJ.setName("jButtonSoignerJ"); // NOI18N
        jButtonSoignerJ.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonSoignerJMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonSoignerJMouseExited(evt);
            }
        });
        jButtonSoignerJ.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSoignerJButtonActionPerformed(evt);
            }
        });

        jButtonNettoyerJ.setText(resourceMap.getString("jButtonNettoyerJ.text")); // NOI18N
        jButtonNettoyerJ.setName("jButtonNettoyerJ"); // NOI18N
        jButtonNettoyerJ.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonNettoyerJMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonNettoyerJMouseExited(evt);
            }
        });
        jButtonNettoyerJ.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonNettoyerJButtonActionPerformed(evt);
            }
        });

        jButtonNourrirV.setText(resourceMap.getString("jButtonNourrirV.text")); // NOI18N
        jButtonNourrirV.setName("jButtonNourrirV"); // NOI18N
        jButtonNourrirV.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonNourrirVMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonNourrirVMouseExited(evt);
            }
        });
        jButtonNourrirV.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonNourrirVButtonActionPerformed(evt);
            }
        });

        jButtonAbreuverV.setText(resourceMap.getString("jButtonAbreuverV.text")); // NOI18N
        jButtonAbreuverV.setName("jButtonAbreuverV"); // NOI18N
        jButtonAbreuverV.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonAbreuverVMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonAbreuverVMouseExited(evt);
            }
        });
        jButtonAbreuverV.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAbreuverVButtonActionPerformed(evt);
            }
        });

        jButtonNettoyerV.setText(resourceMap.getString("jButtonNettoyerV.text")); // NOI18N
        jButtonNettoyerV.setName("jButtonNettoyerV"); // NOI18N
        jButtonNettoyerV.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonNettoyerVMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonNettoyerVMouseExited(evt);
            }
        });
        jButtonNettoyerV.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonNettoyerVButtonActionPerformed(evt);
            }
        });

        jButtonSoignerV.setText(resourceMap.getString("jButtonSoignerV.text")); // NOI18N
        jButtonSoignerV.setName("jButtonSoignerV"); // NOI18N
        jButtonSoignerV.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jButtonSoignerVMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jButtonSoignerVMouseExited(evt);
            }
        });
        jButtonSoignerV.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSoignerVButtonActionPerformed(evt);
            }
        });

        panel1.setName("panel1"); // NOI18N

        jLabel4.setText(resourceMap.getString("jLabel4.text")); // NOI18N
        jLabel4.setName("jLabel4"); // NOI18N

        jLabel5.setText(resourceMap.getString("jLabel5.text")); // NOI18N
        jLabel5.setName("jLabel5"); // NOI18N

        jLabel6.setText(resourceMap.getString("jLabel6.text")); // NOI18N
        jLabel6.setName("jLabel6"); // NOI18N

        jLabelNbMaladesJ.setText(resourceMap.getString("jLabelNbMaladesJ.text")); // NOI18N
        jLabelNbMaladesJ.setName("jLabelNbMaladesJ"); // NOI18N

        jLabelNourrisJ.setText(resourceMap.getString("jLabelNourrisJ.text")); // NOI18N
        jLabelNourrisJ.setName("jLabelNourrisJ"); // NOI18N

        jLabelDesalteresJ.setText(resourceMap.getString("jLabelDesalteresJ.text")); // NOI18N
        jLabelDesalteresJ.setName("jLabelDesalteresJ"); // NOI18N

        javax.swing.GroupLayout panel1Layout = new javax.swing.GroupLayout(panel1);
        panel1.setLayout(panel1Layout);
        panel1Layout.setHorizontalGroup(
            panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panel1Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 47, Short.MAX_VALUE)
                        .addComponent(jLabelDesalteresJ))
                    .addGroup(panel1Layout.createSequentialGroup()
                        .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel6)
                            .addComponent(jLabel5))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelNourrisJ)
                            .addComponent(jLabelNbMaladesJ))))
                .addContainerGap(23, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
        panel1Layout.setVerticalGroup(
            panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(jLabelDesalteresJ))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jLabelNourrisJ))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(jLabelNbMaladesJ))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        panel3.setName("panel3"); // NOI18N

        jLabel12.setText(resourceMap.getString("jLabel12.text")); // NOI18N
        jLabel12.setName("jLabel12"); // NOI18N

        jLabel13.setText(resourceMap.getString("jLabel13.text")); // NOI18N
        jLabel13.setName("jLabel13"); // NOI18N

        jLabel14.setText(resourceMap.getString("jLabel14.text")); // NOI18N
        jLabel14.setName("jLabel14"); // NOI18N

        jLabelNbMaladesL.setText(resourceMap.getString("jLabelNbMaladesL.text")); // NOI18N
        jLabelNbMaladesL.setName("jLabelNbMaladesL"); // NOI18N

        jLabelNourrisL.setText(resourceMap.getString("jLabelNourrisL.text")); // NOI18N
        jLabelNourrisL.setName("jLabelNourrisL"); // NOI18N

        jLabelDesalteresL.setText(resourceMap.getString("jLabelDesalteresL.text")); // NOI18N
        jLabelDesalteresL.setName("jLabelDesalteresL"); // NOI18N

        javax.swing.GroupLayout panel3Layout = new javax.swing.GroupLayout(panel3);
        panel3.setLayout(panel3Layout);
        panel3Layout.setHorizontalGroup(
            panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(panel3Layout.createSequentialGroup()
                        .addComponent(jLabel14)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelNbMaladesL))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, panel3Layout.createSequentialGroup()
                        .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel12)
                            .addComponent(jLabel13))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 44, Short.MAX_VALUE)
                        .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabelNourrisL)
                            .addComponent(jLabelDesalteresL))))
                .addContainerGap(29, Short.MAX_VALUE))
        );
        panel3Layout.setVerticalGroup(
            panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(panel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel12)
                    .addComponent(jLabelDesalteresL))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel13)
                    .addComponent(jLabelNourrisL))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(panel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel14)
                    .addComponent(jLabelNbMaladesL))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1, javax.swing.GroupLayout.DEFAULT_SIZE, 731, Short.MAX_VALUE)
                .addContainerGap())
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel2, javax.swing.GroupLayout.PREFERRED_SIZE, 352, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 37, Short.MAX_VALUE)
                .addComponent(jLabel3, javax.swing.GroupLayout.PREFERRED_SIZE, 352, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGroup(layout.createSequentialGroup()
                .addGap(92, 92, 92)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(imagePanelLapereau, javax.swing.GroupLayout.PREFERRED_SIZE, 230, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jButtonNourrirJ, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonAbreuverJ, javax.swing.GroupLayout.DEFAULT_SIZE, 92, Short.MAX_VALUE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 43, Short.MAX_VALUE)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jButtonNettoyerJ, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonSoignerJ, javax.swing.GroupLayout.DEFAULT_SIZE, 95, Short.MAX_VALUE))))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 131, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addComponent(panel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jButtonNourrirV, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonAbreuverV, javax.swing.GroupLayout.PREFERRED_SIZE, 92, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 43, Short.MAX_VALUE)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                    .addComponent(jButtonNettoyerV, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                    .addComponent(jButtonSoignerV, javax.swing.GroupLayout.DEFAULT_SIZE, 95, Short.MAX_VALUE)))
                            .addComponent(imagePanelLapin, javax.swing.GroupLayout.PREFERRED_SIZE, 230, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(68, 68, 68))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addComponent(panel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addContainerGap())))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jLabel3))
                .addGap(17, 17, 17)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(imagePanelLapin, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(imagePanelLapereau, javax.swing.GroupLayout.PREFERRED_SIZE, 205, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonAbreuverJ)
                            .addComponent(jButtonSoignerJ))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonNettoyerJ)
                            .addComponent(jButtonNourrirJ)))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonAbreuverV)
                            .addComponent(jButtonSoignerV))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jButtonNourrirV)
                            .addComponent(jButtonNettoyerV))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(panel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(panel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(143, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

private void jButtonAbreuverJMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAbreuverJMouseEntered
}//GEN-LAST:event_jButtonAbreuverJMouseEntered

private void jButtonAbreuverJMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAbreuverJMouseExited
}//GEN-LAST:event_jButtonAbreuverJMouseExited

private void jButtonAbreuverJButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAbreuverJButtonActionPerformed
}//GEN-LAST:event_jButtonAbreuverJButtonActionPerformed

private void jButtonNourrirJMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNourrirJMouseEntered
}//GEN-LAST:event_jButtonNourrirJMouseEntered

private void jButtonNourrirJMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNourrirJMouseExited
}//GEN-LAST:event_jButtonNourrirJMouseExited

private void jButtonNourrirJButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNourrirJButtonActionPerformed
}//GEN-LAST:event_jButtonNourrirJButtonActionPerformed

private void jButtonSoignerJMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonSoignerJMouseEntered
}//GEN-LAST:event_jButtonSoignerJMouseEntered

private void jButtonSoignerJMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonSoignerJMouseExited
}//GEN-LAST:event_jButtonSoignerJMouseExited

private void jButtonSoignerJButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSoignerJButtonActionPerformed
}//GEN-LAST:event_jButtonSoignerJButtonActionPerformed

private void jButtonNettoyerJMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNettoyerJMouseEntered
}//GEN-LAST:event_jButtonNettoyerJMouseEntered

private void jButtonNettoyerJMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNettoyerJMouseExited
}//GEN-LAST:event_jButtonNettoyerJMouseExited

private void jButtonNettoyerJButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNettoyerJButtonActionPerformed
}//GEN-LAST:event_jButtonNettoyerJButtonActionPerformed

private void ButtonActionPerformed(java.awt.event.ActionEvent evt) {                                       
    String ActionEvent = evt.getActionCommand();
    
    if(ActionEvent.equals("AbreuverJ"))
    {
    	if(_View._Oracle.callProc("abreuver_clapier", _View._Datas.id_fermier+", TRUE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("AbreuverV"))
    {
    	if(_View._Oracle.callProc("abreuver_clapier", _View._Datas.id_fermier+", FALSE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("NourrirJ"))
    {
    	if(_View._Oracle.callProc("nourrir_clapier", _View._Datas.id_fermier+", TRUE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("NourrirV"))
    {
    	if(_View._Oracle.callProc("nourrir_clapier", _View._Datas.id_fermier+", FALSE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("SoignerJ"))
    {
    	if(_View._Oracle.callProc("soigner_clapier", _View._Datas.id_fermier+", TRUE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("SoignerV"))
    {
    	if(_View._Oracle.callProc("soigner_clapier", _View._Datas.id_fermier+", FALSE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("NettoyerJ"))
    {
    	if(_View._Oracle.callProc("nettoyer_clapier", _View._Datas.id_fermier+", TRUE"))
    	{
    		ReloadDatas();
    	}
    }
    else if(ActionEvent.equals("NettoyerV"))
    {
    	if(_View._Oracle.callProc("nettoyer_clapier", _View._Datas.id_fermier+", FALSE"))
    	{
    		ReloadDatas();
    	}
    }
}
    
private void jButtonNourrirVMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNourrirVMouseEntered
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNourrirVMouseEntered

private void jButtonNourrirVMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNourrirVMouseExited
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNourrirVMouseExited

private void jButtonNourrirVButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNourrirVButtonActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNourrirVButtonActionPerformed

private void jButtonAbreuverVMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAbreuverVMouseEntered
// TODO add your handling code here:
}//GEN-LAST:event_jButtonAbreuverVMouseEntered

private void jButtonAbreuverVMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAbreuverVMouseExited
// TODO add your handling code here:
}//GEN-LAST:event_jButtonAbreuverVMouseExited

private void jButtonAbreuverVButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAbreuverVButtonActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jButtonAbreuverVButtonActionPerformed

private void jButtonNettoyerVMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNettoyerVMouseEntered
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNettoyerVMouseEntered

private void jButtonNettoyerVMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonNettoyerVMouseExited
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNettoyerVMouseExited

private void jButtonNettoyerVButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonNettoyerVButtonActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jButtonNettoyerVButtonActionPerformed

private void jButtonSoignerVMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonSoignerVMouseEntered
// TODO add your handling code here:
}//GEN-LAST:event_jButtonSoignerVMouseEntered

private void jButtonSoignerVMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonSoignerVMouseExited
// TODO add your handling code here:
}//GEN-LAST:event_jButtonSoignerVMouseExited

private void jButtonSoignerVButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSoignerVButtonActionPerformed
// TODO add your handling code here:
}//GEN-LAST:event_jButtonSoignerVButtonActionPerformed

     public void Show()
    {
        setVisible(true);
    	_Field.SetEcus();
        FillDatas();
    }
    
    private void ReloadDatas()
    {
    	_View._Oracle.SetFarmDatas(_View._Datas.nom);
    	_Field.SetEcus();
        FillDatas();
    }
    
    
    private void FillDatas()
    {
      	this.jLabelNbMaladesJ.setText(Integer.toString(_View._Datas.nb_malade_j));
      	this.jLabelNbMaladesL.setText(Integer.toString(_View._Datas.nb_malade_l));
      	this.jLabelDesalteresJ.setText(_View._Datas.abreuvage_j.equals("T") ? "Oui" : "Non");
      	this.jLabelDesalteresL.setText(_View._Datas.abreuvage_l.equals("T") ? "Oui" : "Non");
      	this.jLabelNourrisJ.setText(_View._Datas.nourri_j.equals("T") ? "Oui" : "Non");
      	this.jLabelNourrisL.setText(_View._Datas.nourri_l.equals("T") ? "Oui" : "Non");
        
      	if(_View._Datas.nb_malade_j>0)
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonSoignerJ);
      	else
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonSoignerJ);
        
      	if(_View._Datas.nb_malade_l>0)
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonSoignerV);
      	else
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonSoignerV);
        
      	if(this.jLabelNourrisJ.equals("oui"))
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonNourrirJ);
      	else
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonNourrirJ);
        
      	if(this.jLabelNourrisL.equals("oui"))
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonNourrirV);
      	else
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonNourrirV);
        
      	if(this.jLabelDesalteresJ.equals("oui"))
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonNourrirJ);
      	else
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonNourrirJ);
        
      	if(this.jLabelDesalteresL.equals("oui"))
      		jmeumeu.JMeuMeuHelpers.Disabled(jButtonNourrirV);
      	else
      		jmeumeu.JMeuMeuHelpers.Enabled(jButtonNourrirV);
    }
    
    
    public void Hide()
    {
        setVisible(false);
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private Components.ImagePanel imagePanelLapereau;
    private Components.ImagePanel imagePanelLapin;
    private javax.swing.JButton jButtonAbreuverJ;
    private javax.swing.JButton jButtonAbreuverV;
    private javax.swing.JButton jButtonNettoyerJ;
    private javax.swing.JButton jButtonNettoyerV;
    private javax.swing.JButton jButtonNourrirJ;
    private javax.swing.JButton jButtonNourrirV;
    private javax.swing.JButton jButtonSoignerJ;
    private javax.swing.JButton jButtonSoignerV;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel14;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel22;
    private javax.swing.JLabel jLabel23;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLabel jLabelDesalteresJ;
    private javax.swing.JLabel jLabelDesalteresJ1;
    private javax.swing.JLabel jLabelDesalteresJ3;
    private javax.swing.JLabel jLabelDesalteresJ4;
    private javax.swing.JLabel jLabelDesalteresL;
    private javax.swing.JLabel jLabelNbMaladesJ;
    private javax.swing.JLabel jLabelNbMaladesL;
    private javax.swing.JLabel jLabelNourrisJ;
    private javax.swing.JLabel jLabelNourrisJ1;
    private javax.swing.JLabel jLabelNourrisJ3;
    private javax.swing.JLabel jLabelNourrisJ4;
    private javax.swing.JLabel jLabelNourrisL;
    private java.awt.Panel panel1;
    private java.awt.Panel panel2;
    private java.awt.Panel panel3;
    private java.awt.Panel panel4;
    private java.awt.Panel panel5;
    // End of variables declaration//GEN-END:variables

    private JMeuMeuView _View;
    private jmeumeu.JMeuMeuGameField _Field;
}

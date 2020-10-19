/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jmeumeu;

import java.sql.Date;

/**
 *
 * @author Jeremy
 */
public class JMeuMeuDatas {
//variables du joueur
public String id_fermier;//ID_FERMIER ---login 
public String mdp;// MDP --- password
public String mail;//MAIL
public Date date_enregistrement;// DATE_ENREGISTREMENT
public Date date_connection;// DATE_CONNECTION 

//variables de la ferme
public String nom;//NOM
public int ecus;//ECUS
public int score;//SCORE
public int classement;//CLASSEMENT
public String hibernation; //HIBERNATION -- F ou T
public int nb_achats_jour;//NB_ACHATS_JOUR -- 0<..<12

//variables du poulailler
public int abreuvage_v;
public int nb_abreuvage_v;
public int nourri_v;
public int nb_jeune_v;
public int sale_v;
public int nb_malade_v;
public int poids_v;
public int age_v;
public int production_v;

//variables du clapier
public int nb_male;
public int nb_femelle;
public int nb_gros;
public int nb_moyen;
public int nb_petit;
public String abreuvage_l;
public int nb_abreuvage_l;
public String nourri_l;
public int nb_jeune_l;
public String sale_l;
public int nb_malade_l;
public String abreuvage_j;
public int nb_abreuvage_j;
public String nourri_j;
public int nb_jeune_j;// ???? bizarre nbre de jeune j(jeunes?)
public String sale_j;
public int nb_malade_j;

//variables de la vache
public String abreuvage_vache;
public int nb_abreuvage_vache;
public String nourri_vache;
public int nb_jeune_vache;
public String sale_vache;
public int nb_malade_vache;
public int poids_vache;
public int age_vache;
public int nb_lait;
//variables de l'entrepot
//TODO
}


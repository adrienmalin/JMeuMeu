/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jmeumeu;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.ResultSet;

/**
 *
 * @author Jeremy
 */
public class JMeuMeuOracleAccess {

    private Connection                  _Connection     =   null;
    private Statement                   _Statement      =   null;
    
    //private String                      _url            =   "jdbc:oracle:thin:@panoramix:1521:depinfo";
    private String						_url			=   "jdbc:oracle:thin:@localhost:1521:XE";
    private String                      _Student        =   "etud185";
    private String                      _PwdStudent     =   "9KKFGC38";
    private JMeuMeuDatas				_Datas;
    
    public JMeuMeuOracleAccess()
    {
    	_Datas = new JMeuMeuDatas();
    }
    
    public boolean Connexion()
    {
        // Enregistrement du driver
        try
        { 
                Class.forName ("oracle.jdbc.driver.OracleDriver");
        }
        catch(ClassNotFoundException e)
        {
            JMeuMeuHelpers.ShowErrorMessage("ClassNotFoundException.\n" + e.getMessage());
            return false;
        }
        
        // Connexion
        try
        { 
            // Open Connection
            _Connection = DriverManager.getConnection(_url, _Student, _PwdStudent);
       
            // Open Statement
            _Statement = _Connection.createStatement();
        }
        catch (SQLException ex)
        {
            JMeuMeuHelpers.ShowErrorMessage("   SQLException caught : "+ ex.getMessage()); 
            return false;
        }
        
        return true;
    }
    
    public boolean Deconnexion()
    {
        try
        { 
            // Close the statement
            _Statement.close();
            
            // Close Connection
           _Connection.close();
        } 
        catch (SQLException ex)
        {
            JMeuMeuHelpers.ShowErrorMessage("   SQLException caught : "+ ex.getMessage()); 
            return false;
        }
        
        return true;
    }

    public void QueryUpdate(String Requete)
    {
        try
        {
            // Submit a query
            _Statement.executeUpdate(Requete);
        }
        catch (SQLException ex)
        {
        	JMeuMeuHelpers.ShowErrorMessage("   Query Exception : "+ ex.getMessage()); 
        }
    }

    public ResultSet Query(String Requete)
    {
        ResultSet rs = null;
        
        try
        {
            // Submit a query
            rs = _Statement.executeQuery(Requete);
        } 
        catch (SQLException ex)
        {
            JMeuMeuHelpers.ShowErrorMessage("Query Exception : "+ ex.getMessage()); 
            return null;
        }
        
        return rs;
    }
    
    private ResultSet SelectAllFromFarme(String NAME_FARMER)
    {
    	return Query("SELECT * FROM FERME WHERE NOM='" + NAME_FARMER + "'");
    }
    
    public void SetFarmDatas(String NAME_FARMER)
    {
    	ResultSet RS = SelectAllFromFarme(NAME_FARMER);
    	
    	try
    	{
    		while(RS.next())
    		{
    			_Datas.id_fermier = RS.getString(1);
    			_Datas.nom = RS.getString(2);
    			_Datas.mdp = RS.getString(3);
    			_Datas.mail = RS.getString(4);
    			_Datas.date_enregistrement = RS.getDate(5);
    			_Datas.date_connection = RS.getDate(6);
    			_Datas.ecus = RS.getInt(7);
    			_Datas.score = RS.getInt(8);
    			_Datas.classement = RS.getInt(9);
    			_Datas.hibernation = RS.getString(10);
    			_Datas.nb_achats_jour = RS.getInt(11);
    			_Datas.nb_male = RS.getInt(12);
    			_Datas.nb_femelle = RS.getInt(13);
    			_Datas.nb_gros = RS.getInt(14);
    			_Datas.nb_moyen = RS.getInt(15);
    			_Datas.nb_petit = RS.getInt(16);
    			_Datas.abreuvage_l = RS.getString(17);
    			_Datas.nb_abreuvage_l = RS.getInt(18);
    			_Datas.nourri_l = RS.getString(19);
    			_Datas.nb_jeune_l = RS.getInt(20);
    			_Datas.sale_l = RS.getString(21);
    			_Datas.nb_malade_l = RS.getInt(22);
    			_Datas.abreuvage_j = RS.getString(23);
    			_Datas.nb_abreuvage_j = RS.getInt(24);
    			_Datas.nourri_j = RS.getString(25);
    			_Datas.nb_jeune_j = RS.getInt(26);
    			_Datas.sale_j = RS.getString(27);
    			_Datas.nb_malade_j = RS.getInt(28);
    			_Datas.abreuvage_vache = RS.getString(29);
    			_Datas.nb_abreuvage_vache = RS.getInt(30);
    			_Datas.nourri_vache = RS.getString(31);
    			_Datas.nb_jeune_vache = RS.getInt(32);
    			_Datas.sale_vache = RS.getString(33);
    			_Datas.nb_malade_vache = RS.getInt(34);
    			_Datas.poids_vache = RS.getInt(35);
    			_Datas.age_vache = RS.getInt(36);
    			_Datas.nb_lait = RS.getInt(37);
    		}
    	}
    	catch(SQLException ex)
    	{
    		//JMeuMeuHelpers.ShowError(ex.getMessage());
    		System.err.println(ex.getMessage());
    	}
    }
    
    public JMeuMeuDatas GetDatas()
    {
    	return this._Datas;
    }
    
    public boolean callProc(String proc, String args)
    {
    	try
    	{
    		this._Statement.execute("call "+proc+"('" + args + "')");
    	}
    	catch(SQLException ex)
    	{
    		JMeuMeuHelpers.ShowError(ex.getMessage());
    		return false;
    	}
    	
    	return true;
    }
    
    public ResultSet GetResultProc(String proc, String args)
    {
       try
        {
            // Submit a query
            return _Statement.executeQuery("call "+proc+"("+args+")");
        } 
        catch (SQLException ex)
        {
            JMeuMeuHelpers.ShowErrorMessage("Query Exception : "+ ex.getMessage()); 
            return null;
        }
    }
    
    
}

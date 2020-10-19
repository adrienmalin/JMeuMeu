/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jmeumeu;

import javax.swing.JOptionPane;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 *
 * @author Jeremy
 */
public class JMeuMeuHelpers {

    static public String ToString(char[] S)
    {
        String Result = "";
        for(int i = 0; i < S.length; i++)
            Result += S[i];
        
        return Result;
    }
    
    static public boolean IsEmailAddress(String value)
    {
        Pattern pattern = Pattern.compile("^([a-zA-Z0-9_\\-\\.]+)@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.)|(([a-zA-Z0-9\\-]+\\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\\]?)$");
        Matcher m = pattern.matcher(value);
        
        return m.matches();
    }
    
    static public void ShowErrorMessage(String ErrorMessage)
    {
        JOptionPane.showMessageDialog(null, ErrorMessage, JMeuMeuMessages.MessageBoxError,
                                      JMeuMeuMessages.ERROR);
    }
    
    static public void ShowInformationMessage(String InformationMessage)
    {
        JOptionPane.showMessageDialog(null, InformationMessage, JMeuMeuMessages.MessageBoxInformation,
                                      JMeuMeuMessages.INFORMATION);
    }
    
    static public int ShowQuestionMessage(String QuestionMessage)
    {
        return JOptionPane.showConfirmDialog(null, QuestionMessage, 
                                      JMeuMeuMessages.Message,
                                      JMeuMeuMessages.YES_NO, JMeuMeuMessages.QUESTION);
    }
    
    static public void ShowError(String message)
    {
        int i = Integer.parseInt(message.substring(4, 9));
        
        switch(i)
        {
            case 20001:
                ShowErrorMessage(JMeuMeuMessages.UserNameAlreadyExist);
                break;
                
            case 20002:
                ShowErrorMessage(JMeuMeuMessages.UserNameAlreadyExist);
                break;
                
            case 20003:
                ShowErrorMessage(JMeuMeuMessages.ImpossibleToConnect);
                break;
                
            case 20111:
                ShowErrorMessage(JMeuMeuMessages.NotEnoughMoney);
                break;
                
            case 20112:
                ShowErrorMessage(JMeuMeuMessages.NoCollectable);
                break;
                
            case 20113:
                ShowErrorMessage(JMeuMeuMessages.SendTooArticles);
                break;
                
            case 20114:
                ShowErrorMessage(JMeuMeuMessages.NoRights);
                break;
                
            case 20115:
                ShowErrorMessage(JMeuMeuMessages.OverflowLimit);
                break;
                
            case 20116:
                ShowErrorMessage(JMeuMeuMessages.ClosedCooperative);
            break;
            
            case 20222:
                ShowErrorMessage(JMeuMeuMessages.AnimalsAlreadyEat);
                break;
                
            case 20223:
                ShowErrorMessage(JMeuMeuMessages.AnimalsAlreadyDrink);
                break;
                
            case 20224:
                ShowErrorMessage(JMeuMeuMessages.NotEnoughAnimals);
                break;
                
            case 20225:
                ShowErrorMessage(JMeuMeuMessages.DirtyAnimal);
                break;
                
            case 20226:
                ShowErrorMessage(JMeuMeuMessages.IllAnimal);
                break;
                
            case 20227:
                ShowErrorMessage(JMeuMeuMessages.MaxCapacityReached);
                break;
                
            case 20333:
                ShowErrorMessage(JMeuMeuMessages.NoEgg);
                break;
                
            case 20334:
                ShowErrorMessage(JMeuMeuMessages.LessWeight);
                break;
                
            case 20335:
                ShowErrorMessage(JMeuMeuMessages.TooYoung);
                break;
                
            case 20444:
                ShowErrorMessage(JMeuMeuMessages.NoMilk);  
                break;
                
            case 6576:
            	ShowErrorMessage(JMeuMeuMessages.NoFunctionOrProc);
            	break;
            	
            case 6550:
            	ShowErrorMessage(JMeuMeuMessages.WrongUserName);
            	break;
        }
    }
    
    static public void Enabled(javax.swing.JButton btn)
    {
    	btn.setEnabled(true);
    }
    
    static public void Disabled(javax.swing.JButton btn)
    {
    	btn.setEnabled(false);
    }
    
    static public void ShowDeconnexionError()
    {
        ShowErrorMessage(JMeuMeuMessages.CannotDeconnect);
    }
    
}

package client;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Main {


  public static void main(String[] args) {

    try {
      UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");     // Set GTK+ L&F
    }
    catch (Exception e) {
      System.out.println("Can't load the specified look and feel. Using default.");
    }

    Controller controller = new Controller();
    controller.start();

  }


}

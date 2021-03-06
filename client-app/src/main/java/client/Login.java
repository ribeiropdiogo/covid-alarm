package client;

import client.exceptions.InvalidDistrictException;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;

public class Login {
  private JFrame frame;
  private JPanel panel;
  private JTextField loginNameField;
  private JPasswordField loginPasswordField;
  private JButton loginButton;
  private JLabel nameLabel;
  private JLabel passwordLabel;
  private JTextField registerNameField;
  private JPasswordField registerPasswordField;
  private JPasswordField registerPassword2Field;
  private JComboBox districtField;
  private JButton registerButton;
  private JPanel loginPanel;
  private JPanel registerPanel;
  private JLabel errorLoginLabel;
  private JLabel errorRegisterLabel;
  private JButton selectLocationButton;
  private JLabel selectedLocationLabel;


  public Login(ActionListener login, ActionListener register, ActionListener location) {
    this.loginButton.addActionListener(login);
    this.registerButton.addActionListener(register);
    this.selectLocationButton.addActionListener(location);
  }

  public String getRegisterName() {
    return registerNameField.getText();
  }

  public char[] getRegisterPassword1() {
    return registerPasswordField.getPassword();
  }

  public char[] getRegisterPassword2() {
    return registerPassword2Field.getPassword();
  }

  public int getDistrict() throws InvalidDistrictException {
    switch (districtField.getSelectedItem().toString()) {
      case "Aveiro":
        return 1;
      case "Beja":
        return 2;
      case "Braga":
        return 3;
      case "Bragança":
        return 4;
      case "Castelo Branco":
        return 5;
      case "Coimbra":
        return 6;
      case "Évora":
        return 7;
      case "Faro":
        return 8;
      case "Guarda":
        return 9;
      case "Leiria":
        return 10;
      case "Lisboa":
        return 11;
      case "Portalegre":
        return 12;
      case "Porto":
        return 13;
      case "Santarém":
        return 14;
      case "Setúbal":
        return 15;
      case "Viana do Castelo":
        return 16;
      case "Vila Real":
        return 17;
      case "Viseu":
        return 18;
      default:
        throw new InvalidDistrictException();
    }
  }

  public String getLoginName() {
    return loginNameField.getText();
  }

  public char[] getLoginPassword() {
    return loginPasswordField.getPassword();
  }


  public void setLoginError(String error) {
    errorLoginLabel.setText(error);
    errorLoginLabel.setVisible(true);
  }

  public void setRegisterError(String error) {
    errorRegisterLabel.setText(error);
    errorRegisterLabel.setVisible(true);
  }

  public void clearLoginError() {
    errorLoginLabel.setVisible(false);
    errorLoginLabel.setText("");
  }

  public void clearRegisterError() {
    errorRegisterLabel.setVisible(false);
    errorRegisterLabel.setText("");
  }

  public void setSelectedLocation(int x, int y) {
    selectedLocationLabel.setText("Selecionada localização (" + x + "," + y + ")");
  }

  public void addLoginActionListener(ActionListener listener) {
    loginButton.addActionListener(listener);
  }

  public void addRegisterActionListener(ActionListener listener) {
    registerButton.addActionListener(listener);
  }


  public void start() {
    this.frame = new JFrame("Login");
    frame.setContentPane(this.panel);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack();
    frame.setVisible(true);
  }

  public void quit() {
    frame.setVisible(false);
    frame.dispose();
  }

  {
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
    $$$setupUI$$$();
  }

  /**
   * Method generated by IntelliJ IDEA GUI Designer
   * >>> IMPORTANT!! <<<
   * DO NOT edit this method OR call it in your code!
   *
   * @noinspection ALL
   */
  private void $$$setupUI$$$() {
    panel = new JPanel();
    panel.setLayout(new GridBagLayout());
    panel.setEnabled(true);
    panel.putClientProperty("html.disable", Boolean.FALSE);
    panel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(50, 50, 50, 50), null, TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
    loginPanel = new JPanel();
    loginPanel.setLayout(new GridBagLayout());
    GridBagConstraints gbc;
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 0;
    gbc.gridheight = 10;
    gbc.fill = GridBagConstraints.BOTH;
    panel.add(loginPanel, gbc);
    nameLabel = new JLabel();
    nameLabel.setText("Nome");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 2;
    gbc.anchor = GridBagConstraints.WEST;
    loginPanel.add(nameLabel, gbc);
    loginNameField = new JTextField();
    loginNameField.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 3;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    loginPanel.add(loginNameField, gbc);
    passwordLabel = new JLabel();
    passwordLabel.setText("Palavra-passe");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 5;
    gbc.anchor = GridBagConstraints.WEST;
    loginPanel.add(passwordLabel, gbc);
    final JPanel spacer1 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 4;
    gbc.fill = GridBagConstraints.VERTICAL;
    loginPanel.add(spacer1, gbc);
    loginPasswordField = new JPasswordField();
    loginPasswordField.setEditable(true);
    loginPasswordField.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 6;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    loginPanel.add(loginPasswordField, gbc);
    final JPanel spacer2 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 7;
    gbc.fill = GridBagConstraints.VERTICAL;
    gbc.ipady = 10;
    loginPanel.add(spacer2, gbc);
    loginButton = new JButton();
    loginButton.setText("Entrar");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 8;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    loginPanel.add(loginButton, gbc);
    final JPanel spacer3 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipadx = 400;
    loginPanel.add(spacer3, gbc);
    final JPanel spacer4 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipadx = 20;
    loginPanel.add(spacer4, gbc);
    final JLabel label1 = new JLabel();
    label1.setText("Login");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 0;
    loginPanel.add(label1, gbc);
    final JPanel spacer5 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 9;
    gbc.fill = GridBagConstraints.VERTICAL;
    loginPanel.add(spacer5, gbc);
    errorLoginLabel = new JLabel();
    errorLoginLabel.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 10;
    loginPanel.add(errorLoginLabel, gbc);
    registerPanel = new JPanel();
    registerPanel.setLayout(new GridBagLayout());
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridheight = 10;
    gbc.fill = GridBagConstraints.BOTH;
    panel.add(registerPanel, gbc);
    final JPanel spacer6 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipadx = 400;
    registerPanel.add(spacer6, gbc);
    final JPanel spacer7 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 1;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipadx = 20;
    registerPanel.add(spacer7, gbc);
    final JLabel label2 = new JLabel();
    label2.setText("Nome");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.anchor = GridBagConstraints.WEST;
    registerPanel.add(label2, gbc);
    registerNameField = new JTextField();
    registerNameField.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(registerNameField, gbc);
    final JLabel label3 = new JLabel();
    label3.setText("Palavra-passe");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 5;
    gbc.anchor = GridBagConstraints.WEST;
    registerPanel.add(label3, gbc);
    final JPanel spacer8 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.fill = GridBagConstraints.VERTICAL;
    registerPanel.add(spacer8, gbc);
    registerPasswordField = new JPasswordField();
    registerPasswordField.setEditable(true);
    registerPasswordField.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 6;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(registerPasswordField, gbc);
    final JPanel spacer9 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 17;
    gbc.fill = GridBagConstraints.VERTICAL;
    gbc.ipady = 10;
    registerPanel.add(spacer9, gbc);
    registerButton = new JButton();
    registerButton.setText("Registar");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 18;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(registerButton, gbc);
    final JLabel label4 = new JLabel();
    label4.setText("Repetir palavra-passe");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 8;
    gbc.anchor = GridBagConstraints.WEST;
    registerPanel.add(label4, gbc);
    final JPanel spacer10 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 7;
    gbc.fill = GridBagConstraints.VERTICAL;
    registerPanel.add(spacer10, gbc);
    registerPassword2Field = new JPasswordField();
    registerPassword2Field.setEditable(true);
    registerPassword2Field.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 9;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(registerPassword2Field, gbc);
    final JLabel label5 = new JLabel();
    label5.setText("Distrito");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 11;
    gbc.anchor = GridBagConstraints.WEST;
    registerPanel.add(label5, gbc);
    final JPanel spacer11 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 10;
    gbc.fill = GridBagConstraints.VERTICAL;
    registerPanel.add(spacer11, gbc);
    districtField = new JComboBox();
    districtField.setEditable(false);
    final DefaultComboBoxModel defaultComboBoxModel1 = new DefaultComboBoxModel();
    defaultComboBoxModel1.addElement("");
    defaultComboBoxModel1.addElement("Aveiro");
    defaultComboBoxModel1.addElement("Beja");
    defaultComboBoxModel1.addElement("Braga");
    defaultComboBoxModel1.addElement("Bragança");
    defaultComboBoxModel1.addElement("Castelo Branco");
    defaultComboBoxModel1.addElement("Coimbra");
    defaultComboBoxModel1.addElement("Évora");
    defaultComboBoxModel1.addElement("Faro");
    defaultComboBoxModel1.addElement("Guarda");
    defaultComboBoxModel1.addElement("Leiria");
    defaultComboBoxModel1.addElement("Lisboa");
    defaultComboBoxModel1.addElement("Portalegre");
    defaultComboBoxModel1.addElement("Porto");
    defaultComboBoxModel1.addElement("Santarém");
    defaultComboBoxModel1.addElement("Setúbal");
    defaultComboBoxModel1.addElement("Viana do Castelo");
    defaultComboBoxModel1.addElement("Vila Real");
    defaultComboBoxModel1.addElement("Viseu");
    districtField.setModel(defaultComboBoxModel1);
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 12;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(districtField, gbc);
    final JSeparator separator1 = new JSeparator();
    separator1.setOrientation(1);
    gbc = new GridBagConstraints();
    gbc.gridx = 2;
    gbc.gridy = 0;
    gbc.gridheight = 19;
    gbc.fill = GridBagConstraints.BOTH;
    registerPanel.add(separator1, gbc);
    final JLabel label6 = new JLabel();
    label6.setText("Registar");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 0;
    registerPanel.add(label6, gbc);
    errorRegisterLabel = new JLabel();
    errorRegisterLabel.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 20;
    registerPanel.add(errorRegisterLabel, gbc);
    final JPanel spacer12 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 19;
    gbc.fill = GridBagConstraints.VERTICAL;
    registerPanel.add(spacer12, gbc);
    final JLabel label7 = new JLabel();
    label7.setText("Localização");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 14;
    gbc.anchor = GridBagConstraints.WEST;
    registerPanel.add(label7, gbc);
    final JPanel spacer13 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 13;
    gbc.fill = GridBagConstraints.VERTICAL;
    registerPanel.add(spacer13, gbc);
    selectLocationButton = new JButton();
    selectLocationButton.setText("Selecionar");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 15;
    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    registerPanel.add(selectLocationButton, gbc);
    selectedLocationLabel = new JLabel();
    selectedLocationLabel.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 16;
    registerPanel.add(selectedLocationLabel, gbc);
    nameLabel.setLabelFor(loginNameField);
    passwordLabel.setLabelFor(loginPasswordField);
    label2.setLabelFor(loginNameField);
    label3.setLabelFor(loginPasswordField);
    label4.setLabelFor(loginPasswordField);
    label5.setLabelFor(loginPasswordField);
    label7.setLabelFor(loginPasswordField);
  }

  /**
   * @noinspection ALL
   */
  public JComponent $$$getRootComponent$$$() {
    return panel;
  }

}

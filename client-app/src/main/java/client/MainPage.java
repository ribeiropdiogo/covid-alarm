package client;

import client.exceptions.InvalidDistrictException;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.ActionListener;


public class MainPage {
  private JFrame frame;
  private JPanel panel;
  private JPanel dataPanel;
  private JLabel name;
  private JLabel district;
  private JButton infectedButton;
  private JTextPane warningPane;
  private JLabel nPopulation;
  private JButton selectLocationButton;
  private JButton updateLocationButton;
  private JLabel locationLabel;
  private JLabel updatedLocationLabel;
  private JLabel selectLocationError;
  private JTable locationTable;

  public MainPage(ActionListener selectLocation, ActionListener updateLocation) {
    selectLocationButton.addActionListener(selectLocation);
    updateLocationButton.addActionListener(updateLocation);
  }

  public void start() {
    this.frame = new JFrame("Alarme Covid");
    frame.setContentPane(this.panel);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.pack();
    frame.setVisible(true);
  }

  public void quit() {
    frame.setVisible(false);
    frame.dispose();
  }

  public void setName(String name) {
    this.name.setText(name);
  }

  public void setDistrict(int districtNumber) {
    String district;
    switch (districtNumber) {
      case 1:
        district = "Aveiro";
        break;
      case 2:
        district = "Beja";
        break;
      case 3:
        district = "Braga";
        break;
      case 4:
        district = "Bragança";
        break;
      case 5:
        district = "Castelo Branco";
        break;
      case 6:
        district = "Coimbra";
        break;
      case 7:
        district = "Évora";
        break;
      case 8:
        district = "Faro";
        break;
      case 9:
        district = "Guarda";
        break;
      case 10:
        district = "Leiria";
        break;
      case 11:
        district = "Lisboa";
        break;
      case 12:
        district = "Portalegre";
        break;
      case 13:
        district = "Porto";
        break;
      case 14:
        district = "Santarém";
        break;
      case 15:
        district = "Setúbal";
        break;
      case 16:
        district = "Viana do Castelo";
        break;
      case 17:
        district = "Vila Real";
        break;
      case 18:
        district = "Viseu";
        break;
      default:
        district = "ERRO";
    }
    this.district.setText(district);
  }

  public void setPopulation(int n) {
    this.nPopulation.setText(String.valueOf(n));
  }

  public void setSelectLocationError(String error) {
    this.selectLocationError.setText(error);
  }

  public void setLocationCoordinates(int x, int y) {
    this.locationLabel.setText("População de (" + x + "," + y + "):");
  }

  public void setWarning(String warning) {
    this.warningPane.setText(warning);
  }

  public void setUpdatedLocation(int x, int y) {
    this.updatedLocationLabel.setText("Localização atualizada para (" + x + "," + y + ")");
  }

  public void setUpdatedLocation(String error) {
    this.updatedLocationLabel.setText(error);
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
    panel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(50, 50, 50, 50), null, TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, null, null));
    dataPanel = new JPanel();
    dataPanel.setLayout(new GridBagLayout());
    GridBagConstraints gbc;
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.fill = GridBagConstraints.BOTH;
    panel.add(dataPanel, gbc);
    final JPanel spacer1 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipadx = 400;
    dataPanel.add(spacer1, gbc);
    final JLabel label1 = new JLabel();
    label1.setText("Nome:");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 1;
    gbc.anchor = GridBagConstraints.WEST;
    dataPanel.add(label1, gbc);
    name = new JLabel();
    name.setText("João Carlos");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 2;
    gbc.gridwidth = 6;
    gbc.anchor = GridBagConstraints.WEST;
    dataPanel.add(name, gbc);
    final JLabel label2 = new JLabel();
    label2.setText("Distrito:");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 4;
    gbc.anchor = GridBagConstraints.WEST;
    dataPanel.add(label2, gbc);
    infectedButton = new JButton();
    infectedButton.setText("ESTOU INFETADO");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 13;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.ipady = 10;
    dataPanel.add(infectedButton, gbc);
    warningPane = new JTextPane();
    warningPane.setEditable(false);
    warningPane.setText("Nenhum contacto de risco detetado");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 11;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.BOTH;
    gbc.ipady = 75;
    dataPanel.add(warningPane, gbc);
    final JPanel spacer2 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 3;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.VERTICAL;
    dataPanel.add(spacer2, gbc);
    final JPanel spacer3 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 7;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.VERTICAL;
    dataPanel.add(spacer3, gbc);
    final JPanel spacer4 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 12;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.VERTICAL;
    dataPanel.add(spacer4, gbc);
    district = new JLabel();
    district.setText("Porto");
    gbc = new GridBagConstraints();
    gbc.gridx = 2;
    gbc.gridy = 4;
    gbc.anchor = GridBagConstraints.WEST;
    dataPanel.add(district, gbc);
    locationLabel = new JLabel();
    locationLabel.setText("População de (N/A):");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 5;
    gbc.gridwidth = 3;
    dataPanel.add(locationLabel, gbc);
    final JPanel spacer5 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 3;
    gbc.gridy = 5;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    dataPanel.add(spacer5, gbc);
    selectLocationButton = new JButton();
    selectLocationButton.setText("Selecionar localização");
    gbc = new GridBagConstraints();
    gbc.gridx = 5;
    gbc.gridy = 5;
    dataPanel.add(selectLocationButton, gbc);
    updateLocationButton = new JButton();
    updateLocationButton.setText("Atualizar localização");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 8;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    dataPanel.add(updateLocationButton, gbc);
    final JPanel spacer6 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 10;
    gbc.gridwidth = 6;
    gbc.fill = GridBagConstraints.VERTICAL;
    dataPanel.add(spacer6, gbc);
    updatedLocationLabel = new JLabel();
    updatedLocationLabel.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 0;
    gbc.gridy = 9;
    gbc.gridwidth = 6;
    dataPanel.add(updatedLocationLabel, gbc);
    nPopulation = new JLabel();
    nPopulation.setText("0");
    gbc = new GridBagConstraints();
    gbc.gridx = 4;
    gbc.gridy = 5;
    gbc.anchor = GridBagConstraints.WEST;
    dataPanel.add(nPopulation, gbc);
    selectLocationError = new JLabel();
    selectLocationError.setText("");
    gbc = new GridBagConstraints();
    gbc.gridx = 5;
    gbc.gridy = 6;
    dataPanel.add(selectLocationError, gbc);
    final JPanel spacer7 = new JPanel();
    gbc = new GridBagConstraints();
    gbc.gridx = 1;
    gbc.gridy = 4;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    dataPanel.add(spacer7, gbc);
  }

  /**
   * @noinspection ALL
   */
  public JComponent $$$getRootComponent$$$() {
    return panel;
  }

}

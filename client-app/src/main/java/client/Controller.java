package client;

import client.exceptions.*;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;

public class Controller {

  private static final String QUIT = "/quit";
  private static FrontendServer frontend;
  private PrivNotificationsThread privNotifications;
  private PubNotificationsThread pubNotifications;
  private boolean districtsSubscribed[];
  private Login loginPage;
  private MainPage mainPage;
  private LocationDialog locationDialog;
  private StatisticsDialog statisticsDialog;
  private String username;
  private int userID;
  private int district;
  private int locX, locY;

  public void start() {
    locX = locY = -1;
    try {
      frontend = new FrontendServer();
      privNotifications = new PrivNotificationsThread(this);
      pubNotifications = new PubNotificationsThread(this);
      districtsSubscribed = new boolean[20];
      this.loginPage = new Login(
        new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent actionEvent) {
            login();
          }
        },
        new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent actionEvent) {
            register();
          }
        },
        new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent actionEvent) {
            // Open select location dialog window
            locationDialog = new LocationDialog(e -> {
              locX = locationDialog.getLocX();
              locY = locationDialog.getLocY();
              locationDialog.quit();
              loginPage.setSelectedLocation(locX, locY);
            });
            locationDialog.start();
          }
        }
      );
      loginPage.start();
    } catch (IOException e) {
      System.out.println("Ocorreu um erro inesperado na ligação com o servidor de frontend");
    }
  }

  private void login() {
    String username = loginPage.getLoginName();
    String password = new String(loginPage.getLoginPassword());
    try {
      String[] res = frontend.login(username, password).split(" ");
      this.district = Integer.parseInt(res[0]);
      this.userID = Integer.parseInt(res[1]);
      this.username = username;
      mainPageStart();
    } catch (AlreadyLoggedInException | InvalidParametersException | IOException | NullPointerException e) {
      loginPage.setLoginError("Inválido");
    }
  }

  private void register() {
    String username = loginPage.getRegisterName();
    String password1 = new String(loginPage.getRegisterPassword1());
    String password2 = new String(loginPage.getRegisterPassword2());
    int district;

    try {
      if (username.equals("")) throw new InvalidParametersException();
      if (!password1.equals(password2)) throw new DifferentPasswordsException();
      district = loginPage.getDistrict();
      if (locX == -1 || locY == -1) throw new InvalidParametersException();
      String[] res = frontend.createAccount(username, password1, district, locX, locY).split(" ");
      this.userID = Integer.parseInt(res[1]);
      this.district = district;
      this.username = username;
      mainPageStart();
    } catch (UserExistsException | DifferentPasswordsException | InvalidDistrictException | InvalidParametersException | IOException e) {
      loginPage.setRegisterError(e.getMessage());
    }
  }


  private void mainPageStart() {
    loginPage.quit();
    this.mainPage = new MainPage(
      // Select location
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          locationDialog = new LocationDialog(e -> {
            locX = locationDialog.getLocX();
            locY = locationDialog.getLocY();
            locationDialog.quit();
            try {
              mainPage.setPopulation(frontend.usersInLocation(locX, locY));
              mainPage.setLocationCoordinates(locX, locY);
            } catch (IOException ioException) {
              mainPage.setSelectLocationError("ERRO: a população não foi atualizada");
            }
          });
          locationDialog.start();
        }
      },
      // Update location
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          locationDialog = new LocationDialog(e -> {
            locX = locationDialog.getLocX();
            locY = locationDialog.getLocY();
            locationDialog.quit();
            try {
              frontend.updateLocation(locX, locY);
              mainPage.setUpdatedLocation(locX, locY);
            } catch (IOException | NoUserException exception) {
              mainPage.setUpdatedLocation("ERRO: localização não foi atualizada");
            }
          });
          locationDialog.start();
        }
      },
      // Add stats
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          statisticsDialog = new StatisticsDialog();
          statisticsDialog.start();
        }
      },
      // Add infected
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          try {
            frontend.addInfectedUser();
            mainPage.setInfectedResult("Sucesso! A bloquear...");
            System.exit(0);
          } catch (IOException | NoUserException e) {
            mainPage.setInfectedResult("ERRO: não enviado");
          }
        }
      },
      // Subscribe
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          subscribeToDistrict(mainPage.getSelectedNotificationDistrict());
        }
      },
      // Logout
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          privNotifications.unsubscribe(district, userID);
          unsubscribeAllDistricts();
          frontend.close();
          mainPage.quit();
          // restart
          Controller controller = new Controller();
          controller.start();
        }
      }
    );
    mainPage.setName(this.username);
    mainPage.setDistrict(this.district);
    privNotifications.subscribe(district, userID);
    privNotifications.start();
    pubNotifications.start();
    mainPage.start();
  }


  public void newWarning(String warning) {
    mainPage.setWarning(warning);
  }

  private void subscribeToDistrict(String districtString){
    int district = encodeDistrict(districtString);
    pubNotifications.subscribe(district);
    districtsSubscribed[district] = true;
    mainPage.setNotificationSubscriberMessage("Subscrito.");
  }

  private void unsubscribeAllDistricts(){
    for (int i = 0; i < districtsSubscribed.length; i++) {
      if (districtsSubscribed[i]){
        pubNotifications.unsubscribe(i);
        districtsSubscribed[i] = false;
      }
    }
  }

  public int encodeDistrict(String district) {
    switch (district) {
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
        return 0;
    }
  }


}

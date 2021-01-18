package client;

import client.exceptions.*;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

public class Controller {

  private static final String QUIT = "/quit";
  private static FrontendServer frontend;
  private static PrivNotificationsThread notifications;
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
      notifications = new PrivNotificationsThread(this);
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
    } finally {
      //frontend.close();
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
      notifications.subscribe(district, userID);
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
      notifications.subscribe(district, userID);
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
      // Logout
      new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent actionEvent) {
          notifications.unsubscribe(district, userID);
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
    mainPage.start();
  }

  private void updateStatistics(){
//    statisticsDialog.addNUserDistrictSelectorListener();
  }

  public void newWarning(String warning) {
    mainPage.setWarning(warning);
  }


}

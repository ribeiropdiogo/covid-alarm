package client;

import client.exceptions.*;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Controller {

  private Login loginPage;
  private MainPage mainPage;
  private static FrontendServer frontend;
  private static NotificationsThread notifications;
  private String username;
  private int userID;
  private int district;

  private static final String QUIT = "/quit";

  public void start() {
    try {
      frontend = new FrontendServer();
      notifications = new NotificationsThread(this);
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
        }
      );
      loginPage.start();
    } finally {
      frontend.close();
      notifications.close();
    }
  }

  private void login(){
    String username = loginPage.getLoginName();
    String password = new String(loginPage.getLoginPassword());
    try {
      String[] res = frontend.login(username, password).split(" ");
      this.district = Integer.parseInt(res[0]);
      this.userID = Integer.parseInt(res[1]);
      this.username = username;
      notifications.subscribe(district, userID);
      // TODO - ver isto aqui do unsubscribe
      mainPageStart();
      notifications.unsubscribe(district, userID);
    } catch (AlreadyLoggedInException | InvalidParametersException e) {
      System.out.println(e.getMessage());
    }
  }

  private void register() {
    String username = loginPage.getRegisterName();
    String password1 = new String(loginPage.getRegisterPassword1());
    String password2 = new String(loginPage.getRegisterPassword2());
    int district;
    int locX, locY;

    try {
      if(!password1.equals(password2)) throw new DifferentPasswordsException();
      district = loginPage.getDistrict();
      String[] res = frontend.createAccount(username, password1, district, locX, locY).split(" ");
      this.userID = Integer.parseInt(res[1]);
      this.district = district;
      this.username = username;
      notifications.subscribe(district, userID);
//      // TODO - ver isto aqui do unsubscribe
      mainPageStart();
//      notifications.unsubscribe(district, userID);
    } catch (UserExistsException | DifferentPasswordsException | InvalidDistrictException e) {
      loginPage.setRegisterError(e.getMessage());
    }

  }

  private void mainPageStart(){
    loginPage.quit();
    this.mainPage = new MainPage();

    mainPage.setName(this.username);
    mainPage.setDistrict(this.district);
    mainPage.setUsersInLocation(frontend.usersInLocation()); // TODO - enganei-me aqui. o utilizador pode escolher a localização, não é necessariamente aquela onde ele está

    mainPage.start();
  }

  public void newWarning(String warning){
    mainPage.setWarning(warning);
  }


  private void printLoginData(){
    System.out.println("Login:  "+loginPage.getLoginName()+"   "+new String(loginPage.getLoginPassword()));
  }

  private void printRegisterData(){
    System.out.println("Register:  "+loginPage.getRegisterName()+"   "+new String(loginPage.getRegisterPassword1())+"   "+new String(loginPage.getRegisterPassword2())+"   "+loginPage.getDistrict());
  }

}

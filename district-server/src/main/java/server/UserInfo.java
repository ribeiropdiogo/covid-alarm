package server;

import java.util.ArrayList;
import java.util.List;

public class UserInfo {
    private String userID;
    private int locationX, locationY;
    private List<String> contacts;

    public UserInfo(String id, int x, int y){
        this.userID = id;
        this.locationX = x;
        this.locationY = y;
        this.contacts = new ArrayList<>();
    }

    public String getLocation(){
        return this.locationX+"-"+this.locationY;
    }

    public void updateLocation(int x, int y){
        this.locationX = x;
        this.locationY = y;
    }

    public void addContact(String user){
        this.contacts.add(user);
    }

    public List<String> getContacts(){
        return this.contacts;
    }
}

package server;

import java.util.ArrayList;
import java.util.List;

public class UserInfo {
    private final int userID;
    private int locationX, locationY;
    private final List<Integer> contacts;

    public UserInfo(int id, int x, int y) {
        this.userID = id;
        this.locationX = x;
        this.locationY = y;
        this.contacts = new ArrayList<>();
    }

    public String getLocation() {
        return locationX + "-" + locationY;
    }

    public void updateLocation(int x, int y) {
        this.locationX = x;
        this.locationY = y;
    }

    public void addContact(Integer user) {
        contacts.add(user);
    }

    public List<Integer> getContacts() {
        return contacts;
    }
}

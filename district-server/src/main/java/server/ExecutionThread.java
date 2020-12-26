package server;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

public class ExecutionThread extends Thread {

    private PublicThread publict;
    private PrivateThread privatet;
    private String district, districtn, response;
    private int gridSize, nusers, ninfected;
    private HashMap<String, List<String>> usersByLocation;
    private HashMap<String, UserInfo> users;

    public ExecutionThread(PublicThread pubt, PrivateThread privt,String name, int n, int s){
        this.publict = pubt;
        this.privatet = privt;
        this.district = name;
        this.districtn = String.format("%02d", n);
        this.gridSize = s;
        this.nusers = 0;
        this .ninfected = 0;
        this.usersByLocation = new HashMap<String, List<String>>();

        for (int i = 0; i <= gridSize; i++)
            for (int j = 0; j <= gridSize; j++){
                // Keys X-Y
                this.usersByLocation.put(i+"-"+j,new ArrayList<String>());
            }

        this.users = new HashMap<String, UserInfo>();
    }

    private String addUser(int X, int Y){
        String id = String.valueOf(users.size()+1);
        UserInfo ui = new UserInfo(id,X,Y);

        // Add User to Struct
        if (users.containsKey(ui))
            return "exists";
        else
            users.put(id,ui);

        // Add this users to the map
        this.usersByLocation.get(X+"-"+Y).add(id);

        // Add contacts
        for (String s : this.usersByLocation.get(X+"-"+Y))
            if(s!=id){
                this.users.get(id).addContact(s);
                this.users.get(s).addContact(id);
            }

        nusers++;

        // If there are more than 5 users on a location, notify all
        int us = this.usersByLocation.get(X+"-"+Y).size();
        if (us > 5)
            publict.sendMessage("There are "+us+" users at "+X+"-"+Y);

        return id;
    }

    private String updateLocation(String id, int X, int Y){

        // Checks if user exists
        if (!users.containsKey(id))
            return "no_user";
        else {
            UserInfo u = this.users.get(id);

            // Remove user from previous location
            //System.out.println("> User was "+u.getLocation());
            this.usersByLocation.get(u.getLocation()).remove(id);

            // If there are 0 or less than 5 users on a location, notify all
            if (this.usersByLocation.get(u.getLocation()).size() == 0)
                publict.sendMessage("There are no users at "+u.getLocation());
            else if(this.usersByLocation.get(u.getLocation()).size() < 5)
                publict.sendMessage("There are less than 5 users at "+u.getLocation());

            //Update user's own location
            u.updateLocation(X,Y);
            //System.out.println("> User is "+u.getLocation());

            //Add user on new map location
            this.usersByLocation.get(X+"-"+Y).add(id);

            //Update contacts
            for (String s : this.usersByLocation.get(X+"-"+Y))
                if (s!=id) {
                    this.users.get(id).addContact(s);
                    this.users.get(s).addContact(id);
                }

            // If there are more than 5 users on a location, notify all
            int us = this.usersByLocation.get(u.getLocation()).size();
            if (us > 5)
                publict.sendMessage("There are "+us+" users at "+u.getLocation());

            //System.out.println("> contacts - " + this.users.get(id).getContacts());
            return "success";
        }
    }

    private String getUsersOn(int X, int Y){
        // Return uses in location list
        return String.valueOf(this.usersByLocation.get(X+"-"+Y).size());
    }

    private String addInfected(String id){
        // Checks if user exists
        if (!users.containsKey(id))
            return "no_user";
        else {
            // Notify all of new infection
            publict.sendMessage("There is a new infection in the district.");

            // Notify user that had contact
            for (String s : this.users.get(id).getContacts())
                privatet.sendMessage(s,"You have contacted with an infected user :(");

            ninfected++;
            return "success";
        }
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REP))
        {
            socket.bind("tcp://*:7"+this.districtn+"1");
            while (true) {
                byte[] msg = socket.recv();
                String str = new String(msg);
                String[] parts = str.split("_");

                switch (parts[0]){
                    // nu -> new user
                    // nu_locationX_locationY
                    case "nu":
                        response = addUser(Integer.parseInt(parts[1]), Integer.parseInt(parts[2]));
                        System.out.println("> Created user "+response);
                        socket.send(response);
                        break;
                    // ul -> update location
                    // ul_id_locationX_locationY
                    case "ul":
                        response = updateLocation(parts[1],Integer.parseInt(parts[2]), Integer.parseInt(parts[3]));
                        socket.send(response);
                        break;
                    // us -> users in location
                    // us_locationX_locationY
                    case "us":
                        response = getUsersOn(Integer.parseInt(parts[1]), Integer.parseInt(parts[2]));
                        socket.send(response);
                        break;
                    // ai -> add infected user
                    // ai_id
                    case "ai":
                        response = addInfected(parts[1]);
                        socket.send(response);
                        break;
                    default:
                        socket.send("error");
                }
            }
        }
    }
}

package server;

import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import java.io.IOException;
import java.util.*;


public class ExecutionThread extends Thread {
    
    private final PublicThread pubThread;
    private final PrivateThread privThread;
    private final String distName, distNum;
    private final int gridSize;
    private int nUsers, nInfected, contacts;
    private final HashMap<String, Set<Integer>> usersByLocation;
    private final HashMap<String, Integer> numberOfUsersByLocation;
    private final HashMap<Integer, UserInfo> users;

    
    public ExecutionThread(PublicThread pubThread, PrivateThread privThread,
                           String distName, int distNum, int gridSize) {
        this.pubThread = pubThread;
        this.privThread = privThread;
        this.distName = distName;
        this.distNum = String.format("%02d", distNum);
        this.gridSize = gridSize;
        this.nUsers = 0;
        this.nInfected = 0;
        this.contacts = 0;
        this.usersByLocation = new HashMap<>();
        this.numberOfUsersByLocation = new HashMap<>();

        for (int i = 0; i <= gridSize; i++)
            for (int j = 0; j <= gridSize; j++) {
                // Keys X-Y
                this.usersByLocation.put(i+"-"+j, new HashSet<>());
                this.numberOfUsersByLocation.put(i+"-"+j,0);
            }

        this.users = new HashMap<>();
    }

    private LinkedHashMap<String, Integer> sortUserAmount(){
        LinkedHashMap<String, Integer> sortedMap = new LinkedHashMap<>();

        numberOfUsersByLocation.entrySet()
                .stream()
                .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                .forEachOrdered(x -> sortedMap.put(x.getKey(), x.getValue()));

        return sortedMap;
    }

    private String jsonAux(){
        LinkedHashMap<String, Integer> map = sortUserAmount();
        String s = "";

        int i = 0;

        s+= "   'top5': {\n";

        for (Map.Entry<String, Integer> entry : map.entrySet()) {
            if (i < 4){
                s+= "      '"+entry.getKey()+"': "+entry.getValue()+",\n";
                i++;
            } else if (i == 4){
                s+= "      '"+entry.getKey()+"': "+entry.getValue()+"\n";
                i++;
            }
        }

        s+= "   },\n";

        return s;
    }

    //post of the district
    public void putToDirectory() {
        // Fazer o put
        try {
            CloseableHttpClient httpclient = HttpClients.createDefault();
            HttpPut httpPut = new HttpPut("http://localhost:8080/"+ distName +"/");
            httpPut.setHeader("Accept", "application/json");
            httpPut.setHeader("Content-type", "application/json");

            int avg_contacts = 0;
            if (nInfected > 0) avg_contacts = contacts/ nInfected;



            String json = "{\n" +
                        "   'district': "+distName+",\n" +
                        "   'users':"+nUsers+",\n" +
                        "   'infected':"+nInfected+",\n" +
                        jsonAux() +
                        "   'avg_contacts':"+avg_contacts+"\n" +
                    "}";

            System.out.println(json);

            httpPut.setEntity(new StringEntity(json));
            CloseableHttpResponse response = httpclient.execute(httpPut);
            System.out.println("> PUT response -> " + response.getStatusLine().getStatusCode());
            httpclient.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void incUserInLocation(String key){
        int users = numberOfUsersByLocation.get(key);
        users++;
        numberOfUsersByLocation.put(key,users);
    }

    private void decUserInLocation(String key){
        int users = numberOfUsersByLocation.get(key);
        users--;
        numberOfUsersByLocation.put(key,users);
    }

    private String addUser(int X, int Y) {
        final int id = ++nUsers;
        UserInfo ui = new UserInfo(id, X, Y);

        // Add User to Struct
        users.put(id, ui);

        // Add this user to the map
        usersByLocation.get(X+"-"+Y).add(id);
        incUserInLocation(X+"-"+Y);


        // Add contacts
        for (Integer i : usersByLocation.get(X+"-"+Y))
            if (!i.equals(id)) {
                users.get(id).addContact(i);
                users.get(i).addContact(id);
            }

        // If there are more than 5 users on a location, notify all
        int us = usersByLocation.get(X+"-"+Y).size();
        if (us > 5)
            pubThread.sendMessage("There are "+us+" users at "+X+"-"+Y);

        return String.valueOf(id);
    }

    private String updateLocation(int id, int X, int Y) {
        // Checks if user exists
        if (!users.containsKey(id))
            return "error no_user";
        else {
            UserInfo u = users.get(id);

            // Remove user from previous location
            //System.out.println("> User was "+u.getLocation());
            usersByLocation.get(u.getLocation()).remove(id);
            decUserInLocation(u.getLocation());

            // If there are 0 or less than 5 users on a location, notify all
            if (usersByLocation.get(u.getLocation()).size() == 0)
                pubThread.sendMessage("There are no users at "+u.getLocation());
            else if (usersByLocation.get(u.getLocation()).size() < 5)
                pubThread.sendMessage("There are less than 5 users at "+u.getLocation());

            //Update user's own location
            u.updateLocation(X,Y);
            //System.out.println("> User is "+u.getLocation());

            //Add user on new map location
            usersByLocation.get(X+"-"+Y).add(id);
            incUserInLocation(X+"-"+Y);

            //Update contacts
            for (Integer i : usersByLocation.get(X+"-"+Y))
                if (!i.equals(id)) {
                    users.get(id).addContact(i);
                    users.get(i).addContact(id);
                }

            // If there are more than 5 users on a location, notify all
            int us = usersByLocation.get(u.getLocation()).size();
            if (us > 5)
                pubThread.sendMessage("There are "+us+" users at "+u.getLocation());

            //System.out.println("> contacts - " + users.get(id).getContacts());
            return "ok";
        }
    }

    private String getUsersOn(int X, int Y) {
        // Return uses in location list
        return String.valueOf(usersByLocation.get(X+"-"+Y).size());
    }

    private String addInfected(int id) {
        // Checks if user exists
        if (!users.containsKey(id))
            return "error no_user";
        else {
            // Notify all of new infection
            pubThread.sendMessage("There is a new infection in the district.");

            // Notify user that had contact
            for (Integer i : users.get(id).getContacts()) {
                contacts++;
                privThread.sendMessage(i,"You have contacted with an infected user :(");
            }

            nInfected++;
            return "ok";
        }
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REP))
        {
            String addr = "tcp://*:7" + distNum + "1";
            socket.bind(addr);
            System.out.println("> Socket bind to " + addr);

            while (true) {
                byte[] msg = socket.recv();
                String request = new String(msg);
                System.out.println("> Received: " + request);
                String[] args = request.split(" ");
                String response;

                switch (args[0]) {
                    // nu -> new user
                    // nu <locationX> <locationY>
                    case "nu":
                        response = addUser(Integer.parseInt(args[1]), Integer.parseInt(args[2]));
                        System.out.println("> Created user " + response);
                        socket.send(response);
                        break;
                    // ul -> update location
                    // ul <id> <locationX> <locationY>
                    case "ul":
                        response = updateLocation(Integer.parseInt(args[1]),Integer.parseInt(args[2]), Integer.parseInt(args[3]));
                        socket.send(response);
                        break;
                    // us -> users in location
                    // us <locationX> <locationY>
                    case "us":
                        response = getUsersOn(Integer.parseInt(args[1]), Integer.parseInt(args[2]));
                        socket.send(response);
                        break;
                    // ai -> add infected user
                    // ai <id>
                    case "ai":
                        response = addInfected(Integer.parseInt(args[1]));
                        socket.send(response);
                        break;
                    default:
                        socket.send("error invalid_request");
                }
            }
        }
    }
}

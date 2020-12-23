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
    private String district, districtn;
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

    private int addUser(String id, int X, int Y){
        UserInfo ui = new UserInfo(id,X,Y);

        // Add User to Struct
        if (users.containsKey(ui))
            return -1;
        else
            users.put(id,ui);

        // Add this users to the map
        this.usersByLocation.get(X+"-"+Y).add(id);

        return users.size();
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
                    // nu_id_locationX_locationY
                    case "nu":
                        int response = addUser(parts[1], Integer.parseInt(parts[2]), Integer.parseInt(parts[3]));
                        socket.send(String.valueOf(response));
                        break;
                    default:
                        socket.send("error");
                }
            }
        }
    }
}

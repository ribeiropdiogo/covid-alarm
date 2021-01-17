package client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;


public class PrivNotificationsThread extends Thread {

    private final ZMQ.Socket socket;


    public PrivNotificationsThread() {
        socket = new ZContext().createSocket(SocketType.SUB);
        socket.connect("tcp://localhost:8002");
    }

    public void subscribe(int distNum, int userID) {
        socket.subscribe(String.format("%02d %d ", distNum, userID));
    }

    public void unsubscribe(int distNum, int userID) {
        socket.unsubscribe(String.format("%02d %d ", distNum, userID));
    }

    @Override
    public void run() {
        try {
            while (true) {
                try {
                    String msg = socket.recvStr().split(" ", 3)[2];
                    System.out.println("[!] " + msg);
                } catch (ZMQException e) {
                    break;
                }
            }
        } finally {
            socket.close();
        }
    }
}

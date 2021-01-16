package client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;


public class NotificationsThread extends Thread {

    private final ZMQ.Socket socket;


    public NotificationsThread() {
        socket = new ZContext().createSocket(SocketType.SUB);
        socket.connect("tcp://localhost:8002");
    }

    public void close() {
        socket.close();
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
                String msg = socket.recvStr().split(" ", 3)[2];
                System.out.println("[!] " + msg);
            }
        } finally {
            socket.close();
        }
    }
}

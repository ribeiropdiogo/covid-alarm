package server;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import java.util.LinkedList;
import java.util.Queue;


public class PrivateThread extends Thread {

    private final String distNum;
    private final Queue<String> queue;

    public PrivateThread(int n) {
        this.distNum = String.format("%02d", n);
        this.queue = new LinkedList<>();
    }

    public void sendMessage(int user, String msg) {
        queue.add(user + " " + msg);
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.bind("tcp://*:7" + distNum + "3");
            while (true) {
                if (!queue.isEmpty())
                    socket.send(distNum + " " + queue.remove());
                Thread.sleep(500);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

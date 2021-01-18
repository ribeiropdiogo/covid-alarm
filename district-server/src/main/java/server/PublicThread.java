package server;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import java.util.LinkedList;
import java.util.Queue;


public class PublicThread extends Thread {

    private final String distNum;
    private final Queue<String> queue;

    public PublicThread(int n) {
        this.distNum = String.format("%02d", n);
        this.queue = new LinkedList<>();
    }

    public void sendMessage(String msg) {
        queue.add(msg);
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.bind("tcp://*:7" + distNum + "2");
            while (true) {
                if (!queue.isEmpty()){
                    String s = distNum + " " + queue.remove();
                    System.out.println(s);
                    socket.send(s);
                }
                Thread.sleep(500);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

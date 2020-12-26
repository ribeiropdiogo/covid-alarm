package server;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

public class PublicThread extends Thread {

    private String district, districtn;
    private Queue<String> queue;

    public PublicThread(String name, int n){
        this.district = name;
        this.districtn = String.format("%02d", n);
        this.queue = new LinkedList<>();
    }

    public void sendMessage(String msg){
        this.queue.add(msg);
    }

    public void run() {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.bind("tcp://*:7"+this.districtn+"2");
            while (true) {
                if (!this.queue.isEmpty())
                    socket.send(this.district + "_" + this.queue.remove());
                Thread.sleep(500);
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }


}

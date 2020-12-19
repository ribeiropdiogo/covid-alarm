package server;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Scanner;

public class Test {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.PUB))
        {
            socket.bind("tcp://*:7755");
            Scanner scan= new Scanner(System.in);
            while (true) {
                String str = scan.nextLine();
                if (str == null) break;
                socket.send(str);
            }
        }
    }
}

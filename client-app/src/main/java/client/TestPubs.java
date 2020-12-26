package client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class TestPubs {
    public static void main(String[] args) {
            try (ZContext context = new ZContext();
                 ZMQ.Socket socket = context.createSocket(SocketType.SUB))
            {
                socket.connect("tcp://localhost:7102");
                socket.subscribe("Braga".getBytes());
                //socket.subscribe("Braga_1".getBytes());
            /*
            if (args.length == 1)
                socket.subscribe("".getBytes());
            else for (int i = 1; i < args.length; i++)
                socket.subscribe(args[i].getBytes());
                */
                while (true) {
                    byte[] msg = socket.recv();
                    System.out.println(new String(msg));
                }
            }
        }
    }


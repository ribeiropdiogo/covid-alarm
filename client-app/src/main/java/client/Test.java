package client;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Test {
    public static void main(String[] args) {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REQ))
        {
            socket.connect("tcp://localhost:7101");
            while (true) {
                socket.send("nu_user1_5_4");
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}

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
            socket.send("nu_5_4");
            byte[] msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_5");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_5");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("ul_1_5_5");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("ul_2_5_6");
            msg = socket.recv();
            System.out.println(new String(msg));

            socket.send("us_5_5");
            msg = socket.recv();
            System.out.println("users in 5 5 -> " + new String(msg));

            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));
            socket.send("nu_5_4");
            msg = socket.recv();
            System.out.println(new String(msg));

            socket.send("ai_7");
            msg = socket.recv();
            System.out.println(new String(msg));

        }
    }
}

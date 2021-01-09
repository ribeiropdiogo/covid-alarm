import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;


public class TestCli {
    public static void main(String[] args) throws IOException {
        try (ZContext context = new ZContext();
             ZMQ.Socket socket = context.createSocket(SocketType.REQ))
        {
            socket.connect("tcp://localhost:" + 7011);
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                String str = in.readLine();
                if (str == null) break;
                socket.send(str);
                byte[] msg = socket.recv();
                System.out.println(new String(msg));
            }
        }
    }
}


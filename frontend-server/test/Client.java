import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;


public class Client {

    public static void main(String[] args) throws IOException {
        final Socket socket = new Socket("localhost", 8001);

        System.out.println("Connection established - " + socket.toString());
        
        try {
            final BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
            final BufferedReader sockin = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            final PrintWriter sockout = new PrintWriter(socket.getOutputStream());

            while (true) {
                // Read
                String req = stdin.readLine();
                if (req.equals("/quit")) break;
                // Send
                sockout.println(req);
                sockout.flush();
                // Receive
                String res = sockin.readLine();
                if (res == null) break;
                // Write
                System.out.println(">> " + res);
            }

        } catch (IOException e) {
            System.out.println("Error - " + e.getMessage());

        } finally {
            try {
                socket.shutdownOutput();
                socket.shutdownInput();
                socket.close();

            } catch (IOException e) {
                System.out.println("Connection shutdown/close error - " + e.getMessage());
            }

            System.out.println("Connection finished");
        }
    }
}

package server;

import com.beust.jcommander.JCommander;
import org.zeromq.ZMQ;

public class Server {
    private static Options options = new Options();

    public static void main(String[] args) throws Exception {
        JCommander parser = JCommander.newBuilder()
                .addObject(options)
                .build();
        try {
            parser.parse(args);
            if (options.help) {
                parser.usage();
                return;
            }

            // A partir daqui faz cenas

        } catch(Exception e) {
            parser.usage();
            return;
        }
    }
}

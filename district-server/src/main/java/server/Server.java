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

            System.out.println("> Distric Server '" + options.name + "' started with number " + options.number);

            // Criar as Threads para notificações públicas e privadas
            PublicThread publict = new PublicThread(options.name,options.number);
            PrivateThread privatet = new PrivateThread(options.name,options.number);
            publict.start();
            privatet.start();


        } catch(Exception e) {
            parser.usage();
            return;
        }
    }
}

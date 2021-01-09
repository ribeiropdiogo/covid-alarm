package server;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;


public class Server {
    private static final Options options = new Options();

    public static void main(String[] args) {
        JCommander parser = JCommander.newBuilder()
                .addObject(options)
                .build();
        parser.setProgramName(Server.class.getSimpleName());
        try {
            parser.parse(args);
        } catch(ParameterException e) {
            parser.usage();
            return;
        }

        if (options.help || options.dNumber < 1 || options.dNumber > 18) {
            parser.usage();
            return;
        }

        System.out.println("> Distric Server '" + options.dName + "' started with number " + options.dNumber);

        // Criar as Threads para notificações públicas e privadas
        PublicThread pubThread = new PublicThread(options.dName, options.dNumber);
        PrivateThread privThread = new PrivateThread(options.dName, options.dNumber);
        pubThread.start();
        privThread.start();

        ExecutionThread execThread = new ExecutionThread(pubThread, privThread, options.dName, options.dNumber, options.gSize);
        execThread.start();

        while(true) {
            try {
                Thread.sleep(5000);
                System.out.println("> Sending information to the directory");
                execThread.putToDirectory();
            } catch (InterruptedException ignored) {
            }
        }
    }
}

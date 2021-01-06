package server;

import com.beust.jcommander.JCommander;


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
            //Caso o ip e porta do directorio não sejam passados como parametros
            if (options.directoryAddress == null) {
                options.directoryAddress = "127.0.0.1:8080";
            }

            System.out.println("> Distric Server '" + options.name + "' started with number " + options.number);

            // Criar as Threads para notificações públicas e privadas
            PublicThread publict = new PublicThread(options.name,options.number);
            PrivateThread privatet = new PrivateThread(options.name,options.number);
            publict.start();
            privatet.start();


            ExecutionThread et = new ExecutionThread(publict,privatet,options.name,options.number,options.grid, options.directoryAddress);
            et.start();
            //Cria a thread para fazer de tempos a tempos post da info do directorio
            PushDirectoryThread pdt = new PushDirectoryThread(et, options.directoryAddress);
            pdt.start();

        } catch(Exception e) {
            parser.usage();
            return;
        }
    }
}

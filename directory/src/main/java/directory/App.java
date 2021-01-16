package directory;

import io.dropwizard.Application;
import io.dropwizard.Configuration;
import io.dropwizard.setup.Environment;

public class App extends Application<Configuration> {
    public static void main(String[] args) throws Exception {
        String currentDirectory = System.getProperty("user.dir");
        System.out.println("The current working directory is " + currentDirectory);
        new App().run(args);
    }

    @Override
    public void run(Configuration configuration, Environment environment) throws Exception {
        final DefaultResource defaultResource = new DefaultResource();
        environment.jersey().register(defaultResource);
        environment.healthChecks().register("default", new DefaultHealthCheck());
    }
}
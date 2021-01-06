package server;

import com.beust.jcommander.Parameter;

public class Options {
    @Parameter(names = {"-h","-?","--help"}, help = true, description = "display usage information")
    public boolean help;

    @Parameter(names = {"-n","--name"}, description = "server name")
    public String name;

    @Parameter(names = {"-s","--size"}, description = "size of the NxN grid")
    public int grid = 10;

    @Parameter(names = {"-p","--port"}, description = "port number of the district")
    public int number;

    @Parameter(names = {"-dIp","--directory"}, description = "ip and port number of the directory")
    public String directoryAddress;


}

package server;

import com.beust.jcommander.Parameter;

public class Options {
    @Parameter(names = {"-h", "-?", "--help"}, help = true, description = "display usage information")
    public boolean help;

    @Parameter(names = {"-n", "--name"}, description = "server name")
    public String dName;

    @Parameter(names = {"-d", "--district"}, required = true, description = "number of the district (>=1, <=18)")
    public int dNumber;

    @Parameter(names = {"-s", "--size"}, description = "size of the NxN grid")
    public int gSize = 10;
}

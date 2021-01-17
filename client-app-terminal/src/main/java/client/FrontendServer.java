package client;

import client.exceptions.AlreadyLoggedInException;
import client.exceptions.InvalidParametersException;
import client.exceptions.NoUserException;
import client.exceptions.UserExistsException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;


public class FrontendServer {

    private static Socket socket;
    private static BufferedReader in;
    private static PrintWriter out;


    public FrontendServer() throws IOException {
        socket = new Socket("localhost", 8001);
        in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        out = new PrintWriter(socket.getOutputStream());
    }

    public void close() {
        try {
            socket.close();
        } catch (IOException ignored) {
        }
    }

    public String createAccount(String user, String passwd, int distNum, int locX, int locY) throws IOException, UserExistsException {
        out.println(
            String.format("ca %s %s %02d %d %d", user, passwd, distNum, locX, locY)
        );
        out.flush();

        String res = in.readLine().stripTrailing();

        if (res.equals("error user_exists"))
            throw new UserExistsException();
        else
            return res.split(" ", 2)[1];
    }

    public String login(String user, String passwd) throws IOException, AlreadyLoggedInException, InvalidParametersException {
        out.println(
            String.format("li %s %s", user, passwd)
        );
        out.flush();

        String res = in.readLine().stripTrailing();

        if (res.equals("error already_logged_in"))
            throw new AlreadyLoggedInException();
        else if (res.equals("error invalid"))
            throw new InvalidParametersException();
        else
            return res.split(" ", 2)[1];
    }

    public void logout() throws IOException {
        out.println("lo");
        out.flush();
        in.readLine();
    }

    public void updateLocation(int locX, int locY) throws IOException, NoUserException {
        out.println(
            String.format("ul %d %d", locX, locY)
        );
        out.flush();

        String res = in.readLine().stripTrailing();

        if (res.equals("error no_user"))
            throw new NoUserException();
    }

    public int usersInLocation(int locX, int locY) throws IOException {
        out.println(
            String.format("us %d %d", locX, locY)
        );
        out.flush();

        String res = in.readLine().stripTrailing();

        return Integer.parseInt(res);
    }

    public void addInfectedUser() throws IOException, NoUserException {
        out.println("ai");
        out.flush();

        String res = in.readLine().stripTrailing();

        if (res.equals("error no_user"))
            throw new NoUserException();
    }
}

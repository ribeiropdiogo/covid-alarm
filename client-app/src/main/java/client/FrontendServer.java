package client;

import client.exceptions.AlreadyLoggedInException;
import client.exceptions.InvalidParametersException;
import client.exceptions.NoUserException;
import client.exceptions.UserExistsException;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;


public class FrontendServer {

    private static ZMQ.Socket socket;


    public FrontendServer() {
        socket = new ZContext().createSocket(SocketType.REQ);
        socket.connect("tcp://localhost:8001");
    }

    public void close() {
        socket.close();
    }

    public String createAccount(String user, String passwd, int distNum, int locX, int locY) throws UserExistsException {
        socket.send(
            String.format("ca %s %s %02d %d %d", user, passwd, distNum, locX, locY)
        );

        String res = socket.recvStr();

        if (res.equals("error user_exists"))
            throw new UserExistsException();
        else
            return res.split(" ", 2)[1];
    }

    public String login(String user, String passwd) throws AlreadyLoggedInException, InvalidParametersException {
        socket.send(
            String.format("li %s %s", user, passwd)
        );

        String res = socket.recvStr();

        if (res.equals("error already_logged_in"))
            throw new AlreadyLoggedInException();
        else if (res.equals("error invalid"))
            throw new InvalidParametersException();
        else
            return res.split(" ", 2)[1];
    }

    public void logout() {
        socket.send("lo");
        socket.recvStr();
    }

    public void updateLocation(int locX, int locY) throws NoUserException {
        socket.send(
            String.format("ul %d %d", locX, locY)
        );

        String res = socket.recvStr();

        if (res.equals("error no_user"))
            throw new NoUserException();
    }

    public int usersInLocation(int locX, int locY) {
        socket.send(
            String.format("us %d %d", locX, locY)
        );

        String res = socket.recvStr();

        return Integer.parseInt(res);
    }

    public void addInfectedUser() throws NoUserException {
        socket.send("ai");

        String res = socket.recvStr();

        if (res.equals("error no_user"))
            throw new NoUserException();
    }
}

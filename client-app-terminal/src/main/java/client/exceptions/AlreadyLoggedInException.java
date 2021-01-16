package client.exceptions;

public class AlreadyLoggedInException extends Exception {
    public AlreadyLoggedInException() {
        super("O utilizador já se encontra online");
    }
}

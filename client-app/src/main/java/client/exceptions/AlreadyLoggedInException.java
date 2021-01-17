package client.exceptions;

public class AlreadyLoggedInException extends Exception {
    public AlreadyLoggedInException() {
        super("ERRO: o utilizador já se encontra online");
    }
}

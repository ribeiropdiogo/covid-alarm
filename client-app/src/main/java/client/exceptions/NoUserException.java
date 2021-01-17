package client.exceptions;

public class NoUserException extends Exception {
    public NoUserException() {
        super("O utilizador não existe");
    }
}

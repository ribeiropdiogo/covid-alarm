package client.exceptions;

public class UserExistsException extends Exception {
    public UserExistsException() {
        super("O utilizador já existe");
    }
}

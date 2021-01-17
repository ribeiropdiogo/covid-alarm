package client.exceptions;

public class UserExistsException extends Exception {
    public UserExistsException() {
        super("ERRO: o utilizador já existe");
    }
}

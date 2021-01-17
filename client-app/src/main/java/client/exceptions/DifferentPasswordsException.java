package client.exceptions;

public class DifferentPasswordsException extends Exception {
    public DifferentPasswordsException() {
        super("ERRO: as palavras-passe não coincidem");
    }
}

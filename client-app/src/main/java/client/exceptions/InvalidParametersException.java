package client.exceptions;

public class InvalidParametersException extends Exception {
    public InvalidParametersException() {
        super("ERRO: parâmetros inválidos");
    }
}

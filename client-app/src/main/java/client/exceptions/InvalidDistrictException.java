package client.exceptions;

public class InvalidDistrictException extends Exception {
    public InvalidDistrictException() {
        super("ERRO: o distrito é inválido");
    }
}

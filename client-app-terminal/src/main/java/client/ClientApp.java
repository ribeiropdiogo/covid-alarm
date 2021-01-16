package client;

import client.exceptions.AlreadyLoggedInException;
import client.exceptions.InvalidParametersException;
import client.exceptions.NoUserException;
import client.exceptions.UserExistsException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;


public class ClientApp {

    private static FrontendServer frontend;
    private static NotificationsThread notifications;
    private static BufferedReader stdin;

    private static final String QUIT = "/quit";


    public static void main(String[] args) throws IOException {
        try {
            frontend = new FrontendServer();
            notifications = new NotificationsThread();
            stdin = new BufferedReader(new InputStreamReader(System.in));

            notifications.start();

            mainMenu();
        } finally {
            frontend.close();
            notifications.close();
        }
    }

    private static void mainMenu() throws IOException {
        do {
            System.out.println("1) Iniciar sessão");
            System.out.println("2) Criar conta");
            System.out.println("3) Sair");

            switch (stdin.readLine().strip()) {
                case "1":
                    logIn();
                    break;
                case "2":
                    signUp();
                    break;
                case "3":
                case QUIT:
                    return;
                default:
            }
        } while (true);
    }

    private static void logIn() throws IOException {
        do {
            String username, password;

            do {
                System.out.print("Nome de utilizador: ");
                username = stdin.readLine().strip();
                if (username.equals(QUIT)) return;
                else if (!username.contains(" ")) break;
            } while (true);

            do {
                System.out.print("Password:           ");
                password = stdin.readLine().strip();
                if (password.equals(QUIT)) return;
                else if (!password.contains(" ")) break;
            } while (true);

            try {
                String[] res = frontend.login(username, password).split(" ");
                int distNum = Integer.parseInt(res[0]);
                int userID = Integer.parseInt(res[1]);
                notifications.subscribe(distNum, userID);
                userMenu();
                notifications.unsubscribe(distNum, userID);
                return;
            } catch (AlreadyLoggedInException | InvalidParametersException e) {
                System.out.println(e.getMessage());
            }
        } while (true);
    }

    private static void signUp() throws IOException {
        do {
            String username, password;
            int distNum, locX, locY;

            do {
                System.out.print("Nome de utilizador: ");
                username = stdin.readLine().strip();
                if (username.equals(QUIT)) return;
                else if (!username.contains(" ")) break;
            } while (true);

            do {
                System.out.print("Password:           ");
                password = stdin.readLine().strip();
                if (password.equals(QUIT)) return;
                else if (!password.contains(" ")) break;
            } while (true);

            do {
                System.out.print("Nº do distrito:     ");
                String input = stdin.readLine().strip();
                if (input.equals(QUIT)) return;
                try {
                    distNum = Integer.parseInt(input);
                    break;
                } catch (NumberFormatException ignored) {}
            } while (true);

            do {
                System.out.print("Localização (X):    ");
                String input = stdin.readLine().strip();
                if (input.equals(QUIT)) return;
                try {
                    locX = Integer.parseInt(input);
                    break;
                } catch (NumberFormatException ignored) {}
            } while (true);

            do {
                System.out.print("Localização (Y):    ");
                String input = stdin.readLine().strip();
                if (input.equals(QUIT)) return;
                try {
                    locY = Integer.parseInt(input);
                    break;
                } catch (NumberFormatException ignored) {}
            } while (true);

            try {
                String[] res = frontend.createAccount(username, password, distNum, locX, locY).split(" ");
                int userID = Integer.parseInt(res[1]);
                notifications.subscribe(distNum, userID);
                userMenu();
                notifications.unsubscribe(distNum, userID);
                return;
            } catch (UserExistsException e) {
                System.out.println(e.getMessage());
            }
        } while (true);
    }

    private static void userMenu() throws IOException {
        do {
            System.out.println("1) Atualizar localização");
            System.out.println("2) Nº de utilizadores numa localização");
            System.out.println("3) Adicionar utilizador infetado");
            System.out.println("4) Terminar sessão");

            try {
                switch (stdin.readLine().strip()) {
                    case "1":
                        updateLocation();
                        break;
                    case "2":
                        usersInLocation();
                        break;
                    case "3":
                        frontend.addInfectedUser();
                        break;
                    case "4":
                    case QUIT:
                        frontend.logout();
                        return;
                    default:
                }
            } catch (NoUserException e) {
                System.out.println(e.getMessage());
                return;
            }
        } while (true);
    }

    private static void updateLocation() throws IOException, NoUserException {
        int locX, locY;

        do {
            System.out.print("Localização (X): ");
            String input = stdin.readLine().strip();
            if (input.equals(QUIT)) return;
            try {
                locX = Integer.parseInt(input);
                break;
            } catch (NumberFormatException ignored) {}
        } while (true);

        do {
            System.out.print("Localização (Y): ");
            String input = stdin.readLine().strip();
            if (input.equals(QUIT)) return;
            try {
                locY = Integer.parseInt(input);
                break;
            } catch (NumberFormatException ignored) {}
        } while (true);

        frontend.updateLocation(locX, locY);
    }

    private static void usersInLocation() throws IOException {
        int locX, locY;

        do {
            System.out.print("Localização (X): ");
            String input = stdin.readLine().strip();
            if (input.equals(QUIT)) return;
            try {
                locX = Integer.parseInt(input);
                break;
            } catch (NumberFormatException ignored) {}
        } while (true);

        do {
            System.out.print("Localização (Y): ");
            String input = stdin.readLine().strip();
            if (input.equals(QUIT)) return;
            try {
                locY = Integer.parseInt(input);
                break;
            } catch (NumberFormatException ignored) {}
        } while (true);

        int n = frontend.usersInLocation(locX, locY);
        System.out.println("Número de utilizadores em (" + locX + ", " + locY + "): " + n);
    }
}

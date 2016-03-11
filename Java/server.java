import java.net.*;
import java.io.*;

public class Server {
    public static void main(String[] args) throws IOException {
        // Port Number used
        int port;
        port = 7000;

        // set up port
        int portNumber = Integer.parseInt(Integer.toString(port));
        System.out.println("Listening on port " + portNumber + " . . . ");

        // set up sockets and
        try (
            ServerSocket serverSocket = new ServerSocket(portNumber);
            Socket clientSocket = serverSocket.accept();
            PrintWriter out =
                new PrintWriter(clientSocket.getOutputStream(), true);
            BufferedReader in = new BufferedReader(
                new InputStreamReader(clientSocket.getInputStream()));
        ) {
            // Input & Output strings
            String inputLine, outputLine;

            // Initiate conversation with client
            Protocol p = new Protocol();
            outputLine = p.processInput(null);
            out.println(outputLine);

            // Deal with Input & Output strings
            while ((inputLine = in.readLine()) != null) {
                outputLine = p.processInput(inputLine);
                out.println(outputLine);
                if (outputLine.equals("Bye."))
                    break;
            }
        } catch (IOException e) {
            System.out.println("Exception caught when trying to listen on port "
                + portNumber + " or listening for a connection");
            System.out.println(e.getMessage());
        }
    }
}

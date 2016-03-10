import java.net.*;
import java.io.*;

public class MultiServer {
    public static void main(String[] args) throws IOException {
        // Port Number used
        int port;
        port = 7000;

        // Setup Port
        int portNumber = Integer.parseInt(Integer.toString(port));
        System.out.println("Listening on port " + portNumber + " . . . ");
        boolean listening = true;

        // Setup Socket
        try (ServerSocket serverSocket = new ServerSocket(portNumber)) {
            while (listening) {
                new MultiServerThread(serverSocket.accept()).start();
            }
        } catch (IOException e) {
            System.err.println("Could not listen on port " + portNumber);
            System.exit(-1);
        }
    }
}

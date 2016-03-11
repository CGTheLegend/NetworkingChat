import java.net.*;
import java.io.*;

public class ChatServer implements Runnable{
   private ServerSocket     server = null;
   private Thread           thread = null;
   private ChatServerThread client = null;

   public ChatServer(int port){
     try{
         server = new ServerSocket(port);
         System.out.println("Listening on port " + port);
         start();
      }
      catch(IOException ioe){
        System.out.println(ioe); }
   }

   public void run(){
     while (thread != null){
       try{
         System.out.println("Waiting for a client ...");
            addThread(server.accept());
         }
         catch(IOException ie){
           System.out.println("Acceptance Error: " + ie); }
      }
   }

   public void addThread(Socket socket){
     System.out.println("Client accepted: " + socket);
      client = new ChatServerThread(this, socket);
      try{
         client.open();
         client.start();
      }
      catch(IOException ioe){
        System.out.println("Error opening thread: " + ioe); }
   }
   public void start(){
     if (thread == null){  thread = new Thread(this);
         thread.start();
      }
   }
   public void stop(){
     if (thread != null){  thread.stop();
         thread = null;
      }
   }
   public static void main(String args[]){
      ChatServer server = null;
      int portNumber = 7000;

      server = new ChatServer(portNumber);
   }
}

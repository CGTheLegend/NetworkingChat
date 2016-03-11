import java.io.*;
import java.util.*;
import java.net.*;
import static java.lang.System.out;

public class  Server{

  Vector<String> users = new Vector<String>();
  Vector<HandleClient> clients = new Vector<HandleClient>();

  int PORT = 7000;

  public void process() throws Exception{
      //Setup socket
      ServerSocket server = new ServerSocket(PORT);
      out.println("Listening on port " + PORT + ". . .");

      //Listen for clients
      while( true){
        Socket client = server.accept();
        HandleClient c = new HandleClient(client);
        clients.add(c);
     }
  }

  public static void main(String ... args) throws Exception{
      new Server().process();
  }

  public void boradcast(String user, String message){
    System.out.println(user + " : " + message);
    for (HandleClient c : clients){
      if (!c.getUserName().equals(user)){
        c.sendMessage(user,message);
      }
    }
  }

  class HandleClient extends Thread{
    String name = "";
    BufferedReader input;
    PrintWriter output;

    public HandleClient(Socket client) throws Exception{
         // get input and output streams
         input = new BufferedReader(new InputStreamReader(client.getInputStream())) ;
         output = new PrintWriter (client.getOutputStream(),true);

         // get username
         output.println("Please enter a username: \n");
         name  = input.readLine();
         if(checkUserName(name)){
           output.println("Username already taken \n");
           name = null;
           input = null;
           HandleClient c = new HandleClient(client);
         }
         if(name != null){
           users.add(name);
           output.println("Welcome " + name + "!");
           start();
         }
    }

    public void sendMessage(String uname, String  msg){
        output.println( uname + ":" + msg);
    }

    public String getUserName(){
        return name;
    }

    public Boolean checkUserName(String user){
      for (HandleClient c : clients){
        if (c.getUserName().equals(user)){
          return true;
        }
      }
      return false;
    }

    public void run(){
      String line;

      try{
        while(true){
          line = input.readLine();
          if("EXIT".equals(line)){
            output.println("Closing Connection  . . . Goodbye");
            clients.remove(this);
            users.remove(name);
            break;
          }else if(name.equals(line)){
            output.println("OK");
          }else{
            boradcast(name,line);
          }
        }
      } catch(Exception e) {
           System.out.println(e.getMessage());
      }
    }

  }

}

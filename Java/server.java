// Telnet server
import java.net.*;
import java.io.*;
import java.lang.*;
import java.io.*;
import java.util.*;

class TelnetServer{
    publicstaticvoid main(String args[]) throws Exception{
        System.out.println("Listening on port 7000 . . . ");
        ServerSocket Soc=new ServerSocket(7000);
        while(true){
            Socket CSoc=Soc.accept();
            AcceptTelnetClient ob=new AcceptTelnetClient(CSoc);
        }
    }
}

class AcceptTelnetClient extends Thread{
    Socket ClientSocket;
    DataInputStream dataIn;
    DataOutputStream dataOut;
    String LoginName;

    AcceptTelnetClient(Socket CSoc) throws Exception{
        ClientSocket=CSoc;
        System.out.println("Client Connected ...");
        DataInputStream dataIn=new DataInputStream(ClientSocket.getInputStream());
        DataOutputStream dataOut=new DataOutputStream(ClientSocket.getOutputStream());

        System.out.println("Waiting for UserName");
        LoginName=dataIn.readUTF();
        start();
    }

    publicvoid run(){
        try{
        DataInputStream dataIn=new DataInputStream(ClientSocket.getInputStream());
        DataOutputStream dataOut=new DataOutputStream(ClientSocket.getOutputStream());

        BufferedReader brFin=new BufferedReader(new FileReader("Passwords.txt"));

        String LoginInfo=new String("");
        boolean allow=false;

        while((LoginInfo=brFin.readLine())!=null){
            StringTokenizer st=new StringTokenizer(LoginInfo);
            if(LoginName.equals(st.nextToken()) && Password.equals(st.nextToken())){
                dataOut.writeUTF("ALLOWED");
                allow=true;
                break;
            }
        }

        brFin.close();

        if (allow==false){
            dataOut.writeUTF("NOT_ALLOWED");
        }

        while(allow){
            String strCommand;
            strCommand=dataIn.readUTF();
            if(strCommand.equals("quit")){
                allow=false;
            }else{
                Runtime rt=Runtime.getRuntime();

                Process p=rt.exec("TelnetServer.bat " + strCommand);

                String stdataOut=new String("");
                String st;
                DataInputStream dstdataIn=new DataInputStream(p.getInputStream());
                while((st=dstdataIn.readLine())!=null){
                    stdataOut=stdataOut +st + "\n";
                }
                dstdataIn.close();
                dataOut.writeUTF(stdataOut);
            }
        }
        ClientSocket.close();
        }
        catch(Exception ex){
            ex.printStackTrace();
        }
    }
}

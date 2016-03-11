Cristian Gonzalez - cfgonzal@ucsc.edu
Ian Zentner       - izentner@ucsc.edu

# NetworkingChat
A simple chatting application. Two different versions of the server needed to create a chat room are present. One is written in Python and the other Haskell. Each utilizes the capabilities of telnet.

To connect to any of the servers use the following command:
  telnet localhost <port number>
  
Here the <port number> signifies the port that the server will be listening on. Each should prompt which port it is listening on, but for document purposes they are listed below:
  Haskell: port 5000
  Python : port 6000
  Java   : port 7000

#Haskell Setup
To compile run the following command:
  ghc -o server chat.hs

The executable server under the name "server" and should now be runnable with the following command:
  ./server

#Python Setup
The python code can be executed directly with:
  python server.py

#Java Setup
To compile the java code run:
  javac Server.java
To run the server run:
  java Server

#Resources
http://chimera.labs.oreilly.com/books/1230000000929/ch12.html
https://wiki.haskell.org/Implement_a_chat_server
http://www.tutorialspoint.com/python/python_networking.htm
http://www.binarytides.com/code-chat-application-server-client-sockets-python/
https://docs.oracle.com/javase/tutorial/networking/sockets/clientServer.html
https://docs.python.org/2/library/socket.html
http://www.tutorialspoint.com/java/java_networking.htm
http://stackoverflow.com/questions/9058767/simple-java-chat-server

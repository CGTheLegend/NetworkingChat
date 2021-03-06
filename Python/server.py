import socket, select

# Sends a message to the designamted socket
def broadcast_data (sock, message):
    for socket in CONNECTION_LIST:
        # tests if socket is live
        #  if it is, send message
        #  else close socket
        if socket != server_socket and socket != sock :
            try:
                socket.send(message)
            except:
                socket.close()
                CONNECTION_LIST.remove(socket)
                
if __name__ == "__main__":
    # global data
    CONNECTION_LIST = []
    RECV_BUFFER = 4096
    PORT = 6000
    
    # socket setup
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server_socket.bind(("0.0.0.0", PORT))
    server_socket.listen(10)
    
    # add connection to CONNECTION_LIST
    CONNECTION_LIST.append(server_socket)
    
    user_names = {}
    
    print "Chat server started on port " + str(PORT)
    
    # main server function
    while True:
        read_sockets, write_sockets, error_sockets = select.select(CONNECTION_LIST,[],[])
        
        # adding a new client
        for sock in read_sockets:
            if sock == server_socket:
                sockfd, addr = server_socket.accept()
                CONNECTION_LIST.append(sockfd)
                print "Client (%s, %s) connected" % addr
                user_names[str(sockfd.getpeername())] = None
                sockfd.send("Please enter a username: ")
                #broadcast_data(sockfd, "[%s:%s] entered room\n" % addr)
                
            # send message
            else: 
                try:
                    data = sock.recv(RECV_BUFFER)
                    if data:
                        if user_names[str(sock.getpeername())] is None:
                            user_names[str(sock.getpeername())] = data.rstrip('\r\n')
                            sock.send('Welcome ' + user_names[(str(sock.getpeername()))] + '!\n')
                            broadcast_data(sock, user_names[str(sock.getpeername())] + ' has connected\n')
                        # announce client is leaving and close socket
                        else:
                            broadcast_data(sock, "\r" + '<' + user_names[str(sock.getpeername())] + '> ' + data)
                        
                        #broadcast_data(sock, "\r" + '<' + str(sock.getpeername()) + '>' + data)
                            
                except:
                    broadcast_data(sock, "Client (%s,%s) is offline" % addr)
                    print "Client (%s, %s) is offline" % addr
                    sock.close()
                    CONNECTION_LIST.remove(sock)
                    continue
                        
    server_socket.close()

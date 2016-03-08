
import socket

s = socket.socket()
host = socket.gethostname()
port = 12345

s.connect((host, port))
while True:
	message = s.recv(1024)
	if (message == "disconnect"):
		break
	print message
s.close

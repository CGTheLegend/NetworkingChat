
import socket

s = socket.socket()
host = socket.gethostname()
port = 12345
s.bind((host, port))

s.listen(5)
while True:
	c, addr = s.accept()
	print 'Got connection from', addr
	user_var = raw_input("Message: ")
	while (user_var != "disconnect"):
		c.send(user_var)
		user_var = raw_input("Message: ")
	c.send("disconnect")
	c.close()


import java.net.*;
import java.io.*;
import java.util.*;

public class Protocol {
    // Different states
    private static final int UNREGISTERED = 0;
    private static final int REGISTERING  = 1;
    private static final int REGISTERED   = 2;
    private static final int EXIT         = 3;

    // Starting state
    private int state = UNREGISTERED;

    // Users
    ArrayList<String> users = new ArrayList<String>();
    private String userName;

    // Handle input
    public String processInput(String theInput) {
        String theOutput = null;

        // Unregisterd Case
        if (state == UNREGISTERED) {
            theOutput = "Please enter a username: ";
            state = REGISTERING;

        // Registering Case
        } else if (state == REGISTERING) {
            for (String string : users) {
              if(string.matches(userName)){
                theOutput = "Sorry, " + theInput + " is already taken.";
                state = UNREGISTERED;
                return theOutput;
              }
            }
            userName = theInput;
            users.add(userName);
            state = REGISTERED;
            theOutput = "Welcome, " + userName + "!";
            // broadcastOutput = user + " has connected "

        // Registered Case
        } else if (state == REGISTERED) {
            if (theInput.equalsIgnoreCase("/exit")) {
                theOutput = " Want another? (y/n)";
                state = EXIT;
            } else {
                theOutput = "You're supposed to say \"" +
                " who?\"" +
                "! Try again. Knock! Knock!";
            }
        }
        return theOutput;
    }
}

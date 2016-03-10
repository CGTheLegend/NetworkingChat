import java.net.*;
import java.io.*;

public class Protocol {
    // Different states
    private static final int UNREGISTERED = 0;
    private static final int REGISTERING  = 1;
    private static final int REGISTERED   = 2;
    private static final int EXIT         = 3;

    // Starting state
    private int state = UNREGISTERED;

    private String[] clues = { "Turnip", "Little Old Lady", "Atch", "Who", "Who" };
    private String[] answers = { "Turnip the heat, it's cold in here!",
                                 "I didn't know you could yodel!",
                                 "Bless you!",
                                 "Is there an owl in here?",
                                 "Is there an echo in here?" };

    // Handle input
    public String processInput(String theInput) {
        String theOutput = null;

        if (state == UNREGISTERED) {
            theOutput = "Please enter a username: ";
            state = REGISTERING;
        } else if (state == REGISTERING) {
            if (theInput.equalsIgnoreCase("Who's there?")) {
                theOutput = "Sorry, " + theInput + " is already taken.";
                state = UNREGISTERED;
            } else {
                // user = theInput;
                // userIp = someVar;
                // state = REGISTERED;
                // theOutput = "Welcome, " + user + "!";
                // broadcastOutput = user + " has connected "

                theOutput = "You're supposed to say \"Who's there?\"! " +
                "Try again. Knock! Knock!";
            }
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

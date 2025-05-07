import java.net.Socket;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;

class client {
    public static void main(String[] args) throws IOException {
        if (args.length < 2) {
            System.err.println("Usage: java client <username> <password>");
            return;
        }

        Socket sock = new Socket("localhost", 13556);
        
        PrintWriter out = new PrintWriter(sock.getOutputStream(), true);
        BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        
        String response = in.readLine();

        System.out.println("Logging in as " + args[0]);
        out.println("/l " + args[0] + " " + args[1]);
        response = in.readLine();
        System.out.println("Received: " + response);

        for (int i = 0; i < 3; i++) {
            out.println("/s");

            response = in.readLine();
            System.out.println("Received: " + response);

            while (true) {
                response = in.readLine();
                if (response.equals("END")) {
                    break;
                }
                System.out.println("STREAM: " + response);
            }
            System.out.println("STREAM Ended");

            response = in.readLine();
            System.out.println("Received: " + response);

            out.println("/t");
            response = in.readLine();
            System.out.println("Received: " + response);
        }

        sock.shutdownOutput();
        response = in.readLine();
        System.out.println("Received: " + response);

        sock.close();
    }
}
package com.duelo.client;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

public class AuthManager {
    private static final String USERS_FILE = "users.dat";
    private static final String SESSION_FILE = "user_session.txt";
    private Map<String, String> users;

    public AuthManager() {
        users = new HashMap<>();
        loadUsers();
    }

    private void loadUsers() {
        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(USERS_FILE))) {
            users = (Map<String, String>) ois.readObject();
        } catch (FileNotFoundException e) {
            // File doesn't exist yet, that's okay
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    private void saveUsers() {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(USERS_FILE))) {
            oos.writeObject(users);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public boolean register(String username, String password) {
        if (users.containsKey(username)) {
            return false;
        }
        users.put(username, password);
        saveUsers();
        return true;
    }

    public boolean login(String username, String password) {
        String storedPassword = users.get(username);
        return storedPassword != null && storedPassword.equals(password);
    }

    public boolean unregister(String username) {
        if (!users.containsKey(username)) {
            return false;
        }
        users.remove(username);
        saveUsers();
        return true;
    }

    public static void saveSession(String username) {
        try (FileWriter fw = new FileWriter(SESSION_FILE)) {
            fw.write(username);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String loadSession() {
        try (BufferedReader br = new BufferedReader(new FileReader(SESSION_FILE))) {
            String username = br.readLine();
            if (username != null && !username.trim().isEmpty()) {
                return username.trim();
            }
        } catch (IOException e) {
            // Ignore, session does not exist
        }
        return null;
    }

    public static void clearSession() {
        File f = new File(SESSION_FILE);
        if (f.exists()) {
            f.delete();
        }
    }
} 
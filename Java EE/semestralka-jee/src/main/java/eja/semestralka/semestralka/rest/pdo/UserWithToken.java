package eja.semestralka.semestralka.rest.pdo;

import eja.semestralka.semestralka.rest.entities.User;

public class UserWithToken {
    private long id;
    private String userName;
    private boolean privileged;
    private JwtToken token;

    public UserWithToken(User user, JwtToken jwt) {
        this.id = user.getId();
        this.userName = user.getUserName();
        this.privileged = user.isPrivileged();
        this.token = jwt;
    }

    public long getId() {
        return id;
    }

    public String getUserName() {
        return userName;
    }

    public boolean isPrivileged() {
        return privileged;
    }

    public JwtToken getToken() {
        return token;
    }
}

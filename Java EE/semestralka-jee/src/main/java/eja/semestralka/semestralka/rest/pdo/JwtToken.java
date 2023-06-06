package eja.semestralka.semestralka.rest.pdo;

public class JwtToken {
    private String jwt;

    public JwtToken(String txt) {
        this.jwt = txt;
    }

    public String getJwt() {
        return jwt;
    }

    public void setJwt(String jwt) {
        this.jwt = jwt;
    }
}

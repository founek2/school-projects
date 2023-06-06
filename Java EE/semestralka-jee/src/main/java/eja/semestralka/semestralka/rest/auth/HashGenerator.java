package eja.semestralka.semestralka.rest.auth;


import at.favre.lib.crypto.bcrypt.BCrypt;
import javax.ejb.Stateless;

@Stateless
public class HashGenerator {
    public String createHash(String text) {
        return BCrypt.withDefaults().hashToString(12, text.toCharArray());
    }

    public boolean verify(String plainText, String hash) {
        return BCrypt.verifyer().verify(plainText.toCharArray(), hash).verified;
    }
}

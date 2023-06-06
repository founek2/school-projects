package eja.semestralka.semestralka.rest.auth;

import java.security.*;

import com.nimbusds.jose.*;
import com.nimbusds.jose.crypto.*;
import eja.semestralka.semestralka.rest.pdo.JwtToken;

import javax.annotation.PostConstruct;
import javax.ejb.Singleton;

@Singleton
public class JwtService {
    private byte[] sharedSecret;
    private  JWSSigner signer;

    @PostConstruct
    public void init() {
        SecureRandom random = new SecureRandom();
        sharedSecret = new byte[32];
        random.nextBytes(sharedSecret);

        // Create HMAC signer
        try {
            signer = new MACSigner(sharedSecret);
        }catch (Exception e) {

        }
    }

    public JwtToken createToken(String userName) throws KeyLengthException, JOSEException {
        // Prepare JWS object with "Hello, world!" payload
        JWSObject jwsObject = new JWSObject(new JWSHeader(JWSAlgorithm.HS256), new Payload(userName));

        // Apply the HMAC
        jwsObject.sign(signer);
        return new JwtToken(jwsObject.serialize());
    }

    public Payload verify(String token) throws Exception {
        JWSObject jwsObject;

        // To parse the JWS and verify it, e.g. on client-side
        jwsObject = JWSObject.parse(token);

        JWSVerifier verifier = new MACVerifier(sharedSecret);

        if (!jwsObject.verify(verifier))
            throw new InvalidTokenException();
        //assertTrue(jwsObject.verify(verifier));

        //assertEquals("Hello, world!", jwsObject.getPayload().toString());
        return jwsObject.getPayload();
    }

}

package eja.semestralka.semestralka.rest.auth;

import com.nimbusds.jose.Payload;
import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.persistance.UserJPA;

import javax.inject.Inject;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.ext.Provider;
import java.io.IOException;
import java.security.Principal;
import java.util.Optional;
import java.util.logging.Logger;

@Provider
@JwtAuth
public class JwtAuthFilter implements ContainerRequestFilter {
    @Inject
    JwtService jwt;

    @Inject
    UserJPA userJPA;

    private static final Logger LOG = Logger.getLogger(JwtAuthFilter.class.getName());

    @Override
    public void filter(ContainerRequestContext reqContext) throws IOException {
        String token = reqContext.getHeaders().get("Auth-Token").get(0);
        try {
            Payload p = jwt.verify(token);
            Optional<User> userOpt = userJPA.findByName(p.toString());
            if (!userOpt.isPresent())
                throw new Exception();

            LOG.info("User id> " + p.toString());

            reqContext.setSecurityContext(new SecurityContext() {
                                              @Override
                                              public Principal getUserPrincipal() {
                                                  return p::toString;
                                              }

                                              @Override
                                              public boolean isUserInRole(String role) {
                                                  return role.equals("admin") && userOpt.get().isPrivileged();
                                              }

                                              @Override
                                              public boolean isSecure() {
                                                  return false;
                                              }

                                              @Override
                                              public String getAuthenticationScheme() {
                                                  return "Token-Based-Auth-Scheme";
                                              }

                                          }
            );
        } catch (Exception e) {
            reqContext.abortWith(Response.status(403).build());
        }
    }
}

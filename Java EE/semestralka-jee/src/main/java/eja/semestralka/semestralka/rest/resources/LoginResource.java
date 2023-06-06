package eja.semestralka.semestralka.rest.resources;

import eja.semestralka.semestralka.rest.auth.HashGenerator;
import eja.semestralka.semestralka.rest.auth.JwtService;
import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.pdo.JwtToken;
import eja.semestralka.semestralka.rest.pdo.UserWithToken;
import eja.semestralka.semestralka.rest.persistance.UserJPA;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Optional;

@Path(value = "/login")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class LoginResource {
    @Inject
    UserJPA userJPA;

    @Inject
    HashGenerator hashGen;

    @Inject
    JwtService jwt;

    @POST
    public Response login(@Valid User user) {
        try {
            Optional<User> opt = userJPA.findByName(user.getUserName());

            if (opt.isPresent()) {
                if (hashGen.verify(user.getPasswd(), opt.get().getPasswd())) {

                    JwtToken token = jwt.createToken(user.getUserName());
                    return Response.ok(new UserWithToken(opt.get(), token)).build();
                }
            }
        } catch (Exception e) {
        }


        return Response.status(403).build();
    }
}

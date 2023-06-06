package eja.semestralka.semestralka.rest.resources;

import eja.semestralka.semestralka.rest.auth.HashGenerator;
import eja.semestralka.semestralka.rest.auth.JwtAuth;
import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.persistance.UserJPA;

import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import java.util.List;

@Path("/users")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class UsersResource {
    @Inject
    UserJPA userJPA;

    @Inject
    HashGenerator hashGen;

    @GET
    public List<User> getAll() {
        return userJPA.all();
    }

    @DELETE
    @JwtAuth
    public Response delete(List<User> users, @Context SecurityContext securityContext) {
        String userName = securityContext.getUserPrincipal().getName();

        if(users.size() == 1 && users.get(0).getUserName().equals(userName)) {
            userJPA.deleteByName(userName);
        } else if (securityContext.isUserInRole("admin"))
            userJPA.delete(users);
        else return Response.status(403).build();

        return Response.ok().build();
    }

    @PATCH
    @JwtAuth
    @Path(value = "/{id}")
    public Response update(User user, @PathParam("id") long userId, @Context SecurityContext securityContext) {
        System.out.println(user.toString());
        if (securityContext.isUserInRole("admin")) {
            if (user.getPasswd() != null){
                int len = user.getPasswd().length();
                if (len >= 4 && len <= 40)
                     user.setPasswd(hashGen.createHash(user.getPasswd()));
                else
                    return Response.status(400).build();
            }

            userJPA.update(userId, user);
        } else return Response.status(403).build();

        return Response.ok().build();
    }
}

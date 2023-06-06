package eja.semestralka.semestralka.rest.resources;

import eja.semestralka.semestralka.rest.auth.HashGenerator;
import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.persistance.UserJPA;

import javax.ejb.EJBTransactionRolledbackException;
import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;

@Path(value = "/registration")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class RegistrationResource {
    @Inject
    UserJPA userJPA;

    @Inject
    HashGenerator hashGen;

    @POST
    public Response create(@Valid User user) {
        user.setPasswd(hashGen.createHash(user.getPasswd()));

        try {
            if (!userJPA.adminExists())
                user.setPrivileged(true);

            userJPA.create(user);
            System.out.println(user.toString());
        } catch (EJBTransactionRolledbackException e) {
            return Response.status(400).build();
        }

        return Response.ok().build();
    }

    @GET
    public List<User> getAll() {
        return userJPA.all();
    }
}

package eja.semestralka.semestralka.rest.resources;

import eja.semestralka.semestralka.rest.auth.JwtAuth;
import eja.semestralka.semestralka.rest.entities.TodoItem;
import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.persistance.TodoItemJPA;
import eja.semestralka.semestralka.rest.persistance.UserJPA;

import javax.inject.Inject;
import javax.validation.Valid;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;
import java.util.Collection;
import java.util.Optional;

@JwtAuth
@Path(value = "/todo")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class TodoResource {
    @Inject
    UserJPA userJPA;
    @Inject
    TodoItemJPA todoJPA;

    @POST
    public TodoItem create(@Valid TodoItem todo, @Context SecurityContext security) {
        String userName = security.getUserPrincipal().getName();
        Optional<User> op = userJPA.findByName(userName);

        todo.setOwner(op.get());
        return todoJPA.create(todo);
    }

    @PUT
    @Path(value = "/{id}")
    public void update(@Valid TodoItem todo, @PathParam("id") long todoId, @Context SecurityContext security) {
        String userName = security.getUserPrincipal().getName();
        Optional<User> op = userJPA.findByName(userName);
        todo.setId(todoId);
        todo.setOwner(op.get());
        todoJPA.update(todo);
    }

    @DELETE
    public void delete(TodoItem todo, @Context SecurityContext security) {
        String userName = security.getUserPrincipal().getName();
        Optional<User> op = userJPA.findByName(userName);

        todo.setOwner(op.get());
        todoJPA.delete(todo);
    }

    @GET
    public Collection<TodoItem> getAll(@Context SecurityContext security) {
        String userName = security.getUserPrincipal().getName();

        return userJPA.allItems(userName);
    }
}

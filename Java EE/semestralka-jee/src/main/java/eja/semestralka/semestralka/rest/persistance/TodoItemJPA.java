package eja.semestralka.semestralka.rest.persistance;

import eja.semestralka.semestralka.rest.entities.TodoItem;
import eja.semestralka.semestralka.rest.entities.User;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import java.util.List;

@Stateless
public class TodoItemJPA {
    @PersistenceContext
    EntityManager em;

    public TodoItem create(TodoItem todoItem) {
        em.persist(todoItem);
        em.flush();
        return todoItem;
    }

    public void update(TodoItem todoItem){
        em.merge(todoItem);
    }

    public void delete(TodoItem todoItem){
        TodoItem item = em.find(TodoItem.class, todoItem.getId());
        em.remove(item);
    }

    public List<TodoItem> all(User user) {
      Query tq = em.createQuery("select i from TodoItem i where i.owner.id = :userId");
      tq.setParameter("userId", user.getId());
      return tq.getResultList();
    }
}

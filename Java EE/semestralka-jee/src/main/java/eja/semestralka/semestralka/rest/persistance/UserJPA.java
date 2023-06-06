package eja.semestralka.semestralka.rest.persistance;

import eja.semestralka.semestralka.rest.entities.TodoItem;
import eja.semestralka.semestralka.rest.entities.User;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

@Stateless
public class UserJPA {
    @PersistenceContext
    EntityManager em;

    public void create(User usr) {
        em.persist(usr);
        em.flush();
    }

    public List<User> all() {
        TypedQuery<User> tq = em.createNamedQuery("all", User.class);
        return tq.getResultList();
    }

    public Optional<User> findByName(String userName) {
        TypedQuery<User> tq = em.createNamedQuery("findByName", User.class);
        tq.setParameter("name", userName);
        Optional<User> out = Optional.of(tq.getSingleResult());
        return out;
    }

    public Collection<TodoItem> allItems(String userName) {
        // calling findByName cause close of transcation -> layzload is not working
        TypedQuery<User> tq = em.createNamedQuery("findByName", User.class);
        tq.setParameter("name", userName);
        return tq.getSingleResult().getTodoItems();
    }

    public boolean adminExists() {
        try {
            return em.createQuery("select u from User u where u.privileged = true").getResultList().size() > 0;
        } catch (Exception e) {
            return false;
        }
    }

    public void delete(List<User> users) {
        for (User u : users) {
            User founded = em.find(User.class, u.getId());
            em.remove(founded);
        }
    }

    public void deleteByName(String userName) {
        TypedQuery<User> tq = em.createNamedQuery("findByName", User.class);
        tq.setParameter("name", userName);
        em.remove(tq.getSingleResult());

    }

    public void update(long userId, User user) {

        User found = em.find(User.class, userId);
        found.setPrivileged(user.isPrivileged());
        if (user.getPasswd() != null) found.setPasswd(user.getPasswd());

        em.merge(found);
    }
}

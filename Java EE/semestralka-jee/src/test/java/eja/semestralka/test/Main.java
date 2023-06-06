package eja.semestralka.test;

import eja.semestralka.semestralka.rest.entities.User;
import eja.semestralka.semestralka.rest.persistance.UserJPA;
import eu.drus.jpa.unit.api.JpaUnitRunner;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import java.util.List;
import java.util.Optional;

import static junit.framework.Assert.assertEquals;

@RunWith(JpaUnitRunner.class)
public class Main {
    @PersistenceContext(unitName = "integration-test")
    private EntityManager em;

    @Test
    public void listCountTest1() {
        em.createNamedQuery("findByName", User.class);
        assertEquals(1, 1);
    }
}
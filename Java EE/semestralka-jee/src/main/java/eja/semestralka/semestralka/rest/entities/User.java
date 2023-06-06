package eja.semestralka.semestralka.rest.entities;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@NamedQuery(name = "all", query = "select u from User u")

@NamedQuery(name = "findByName", query = "select u from User u where u.userName = :name")

@Entity
@Table(name = "Users")
public class User implements Serializable {

    @Id
    @GeneratedValue
    private long id;

    @Column(unique = true)
    @NotBlank
    @Size(min = 4, max = 40)
    private String userName;

    @NotBlank
    @JsonIgnore
    private String passwd;

    @JsonIgnore
    @OneToMany(mappedBy = "owner", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<TodoItem> todoItems = new ArrayList<TodoItem>();

    @Column
    private boolean privileged;

    public User() {
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    @JsonIgnore
    public String getPasswd() {
        return passwd;
    }

    @JsonProperty
    public void setPasswd(String passwd) {
        this.passwd = passwd;
    }

    public Collection<TodoItem> getTodoItems() {
        return todoItems;
    }

    public void setTodoItems(List<TodoItem> todoItems) {
        this.todoItems = todoItems;
    }

    public boolean isPrivileged() {
        return privileged;
    }

    public void setPrivileged(boolean privileged) {
        this.privileged = privileged;
    }

    @Override
    public String toString() {
        return "User{" +
                "id=" + id +
                ", userName='" + userName + '\'' +
                ", passwd='" + passwd + '\'' +
                '}';
    }
}

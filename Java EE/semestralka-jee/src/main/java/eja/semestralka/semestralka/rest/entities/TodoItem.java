package eja.semestralka.semestralka.rest.entities;

import com.fasterxml.jackson.annotation.JsonIgnore;
import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Calendar;

@Entity
@Table(name = "TodoItems")
public class TodoItem implements Serializable {
    @Id
    @GeneratedValue
    private long id;

    @NotBlank
    @Size(min = 1, max = 300)
    private String text;

    @Column
    private boolean done = false;

    @ManyToOne
    @JsonIgnore
    private User owner;

    @Column(columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP", insertable=false, updatable=false)
    private Calendar dateTime;


    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public User getOwner() {
        return owner;
    }

    public void setOwner(User owner) {
        this.owner = owner;
    }

    public boolean isDone() {
        return done;
    }

    public void setDone(boolean done) {
        this.done = done;
    }

    public Calendar getDateTime() {
        return dateTime;
    }


    public void setDateTime(Calendar dateTime) {
        this.dateTime = dateTime;
    }
}

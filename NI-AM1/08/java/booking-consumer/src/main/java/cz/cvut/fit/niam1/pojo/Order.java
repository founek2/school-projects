package cz.cvut.fit.niam1.pojo;

import java.io.Serializable;

public class Order implements Serializable  {

    private Long id;
    private String content;

    public Order(final Long id, final String content){
        this.id = id;
        this.content = content;
    }

    public Long getId() {
        return id;
    }

    public String getContent() {
        return content;
    }
}

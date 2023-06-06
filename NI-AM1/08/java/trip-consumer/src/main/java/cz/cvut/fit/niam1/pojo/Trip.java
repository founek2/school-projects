package cz.cvut.fit.niam1.pojo;

import java.io.Serializable;

public class Trip implements Serializable {
    private Long id;
    private String destination;

    public Trip(Long id, String destination) {
        this.id = id;
        this.destination = destination;
    }

    public Long getId() {
        return id;
    }

    public String getDestination() {
        return destination;
    }
}

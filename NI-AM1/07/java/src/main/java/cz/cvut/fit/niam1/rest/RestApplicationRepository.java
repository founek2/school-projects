package cz.cvut.fit.niam1.rest;


import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
public class RestApplicationRepository {

    private static final List<Country> countries = new ArrayList<>();
    private static final List<Tour> tours = new ArrayList<>();
    private final ZonedDateTime lastModified = ZonedDateTime.now();


    @PostConstruct
    public void initRepo() {
        countries.add(new Country("cz", "Czech Republic"));
        countries.add(new Country("d", "Germany"));
        tours.add(new Tour("1", "Beach tour"));
        tours.add(new Tour("2", "Ski tour"));
    }

    public void addCountry(Country c) {
        countries.add(c);
    }

    public List<Country> getCountries() {
        return countries;
    }

    public Country getCountryById(String id) {
        return countries.stream().filter(c -> c.getId().equals(id)).findAny().orElse(null);
    }

    public void deleteCountry(String id) {
        countries.removeIf(c -> c.getId().equals(id));
    }

    public void addTour(Tour t) {    
        tours.add(t);
    }

    public List<Tour> getTours() {
        return tours;
    }

    public Tour getTourById(String id) {
        return tours.stream().filter(c -> c.getId().equals(id)).findAny().orElse(null);
    }

    public void deleteTour(String id) {
        tours.removeIf(c -> c.getId().equals(id));
    }

    public ZonedDateTime getLastModified() {
        return lastModified;
    }
}

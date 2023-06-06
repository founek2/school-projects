package cz.cvut.fit.niam1.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


@RestController
public class RestApplicationController {

    @Autowired
    RestApplicationRepository repository;

    @GetMapping("/country/{id}")
    @ResponseStatus(HttpStatus.OK)
    public Country getCountry(@PathVariable String id) {
        return repository.getCountryById(id);
    }

    @PostMapping(value = "/country")
    public ResponseEntity createCountry(@RequestBody Country country) {
        repository.addCountry(country);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping("/tour/{id}/confirmation")
    @ResponseStatus(HttpStatus.OK)
    public String tourConfirmation(@PathVariable String id) {
        return repository.newNonce(id);
    }

    @DeleteMapping("/tour/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public ResponseEntity getTour(@RequestParam String nonce, @PathVariable String id) {
        boolean validNonce = repository.validateAndUseNonce(nonce);
        if (validNonce) {
            repository.deleteTour(id);
            return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
        }

        return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
    }

    @PostMapping(value = "/tour")
    public ResponseEntity createTour(@RequestBody Tour tour) {
        repository.addTour(tour);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @GetMapping(value = "/tour")
    public List<Tour> listTours() {
        return repository.getTours();
    }
}

package cz.cvut.fit.niam1.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.CacheControl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.DigestUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.WebRequest;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;


@RestController
public class RestApplicationController {

    @Autowired
    RestApplicationRepository repository;

    @GetMapping("/tour")
    @ResponseStatus(HttpStatus.OK)
    public List<Tour> getTours() {
        return repository.getTours();
    }

    private String computeWeakETagValue(List<Tour> tours) {
        StringBuilder builder = new StringBuilder(37);

            builder.append("W/");

        builder.append("\"0");
        String str = tours.stream().map(c -> c.getId() + c.getName()).collect(Collectors.joining());
        DigestUtils.appendMd5DigestAsHex(str.getBytes(), builder);
        builder.append('"');
        return builder.toString();

    }

    @GetMapping("/tour2")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity getTours2(@RequestHeader(value = "if-none-match", required = false, defaultValue = "") String etagMatchHeader) {
        List<Tour> tours = repository.getTours();

        final String eTag = computeWeakETagValue(tours);
        if (etagMatchHeader.equals(eTag)) return ResponseEntity.status(HttpStatus.NOT_MODIFIED).build();

        return ResponseEntity.status(HttpStatus.OK).lastModified(repository.getLastModified()).eTag(eTag).body(tours);
    }

    @PostMapping("/tour")
    @ResponseStatus(HttpStatus.OK)
    public ResponseEntity createTour(@RequestBody Tour t) {
         repository.addTour(t);

         return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}

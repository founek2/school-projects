package cz.cvut.fit.niam1.rest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.http.*;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import javax.servlet.http.HttpServletRequest;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;


@RestController
public class RestApplicationController {
    private final Logger logger = LoggerFactory.getLogger(RestApplicationController.class);

    final List<URI> upstreams = new ArrayList<URI>() {
        {
            try {
                add(new URI("http://147.32.233.18:8888/MI-MDW-LastMinute1/list"));
                add(new URI("http://147.32.233.18:8888/MI-MDW-LastMinute2/list"));
                add(new URI("http://147.32.233.18:8888/MI-MDW-LastMinute3/list"));
            } catch (URISyntaxException e) {
                e.printStackTrace();
            }
        }
    };

    Map<URI, URI> healthy = new HashMap<>();


    @Bean
    public RestTemplate restTemplate() {
        return new RestTemplate();
    }

    @Autowired
    RestTemplate restTemplate;


   @Scheduled(fixedRate = 5 * 1000)
    public void healthCheck() throws URISyntaxException {
        for(URI url : upstreams){
            try {
                ResponseEntity<String> responseEntity = restTemplate.exchange(url, HttpMethod.GET, null, String.class);
                if (responseEntity.getStatusCode().value() == 200)
                    healthy.put(url, url);
                else healthy.remove(url);
            }catch(HttpServerErrorException err) {
                healthy.remove(url);
            }
        }
       logger.info("health check: {} alive", healthy.size());
    }

    URI getRandomHealthyUri() {
        List<URI> valuesList = new ArrayList<>(healthy.values());
        if (valuesList.size() == 0) return null;

        int randomIndex = new Random().nextInt(valuesList.size());
        return valuesList.get(randomIndex);
    }


    @GetMapping(value = "/test")
    public ResponseEntity<String> test(HttpServletRequest request) {
        URI uri = getRandomHealthyUri();
        if (uri == null) return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).build();

        logger.info("using upstream: {}", uri.toString());

        // copy headers
        HttpHeaders headers = new HttpHeaders();
        Collections.list(request.getHeaderNames()).forEach(head -> headers.add(head, request.getHeader(head)));

        // create request entity
        HttpEntity<String> requestEntity = new HttpEntity<>(headers);

        // HTTP
        ResponseEntity<String> responseEntity = restTemplate.exchange(uri, HttpMethod.GET, requestEntity, String.class);
        return responseEntity;
    }

}

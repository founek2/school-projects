package cz.cvut.fit.niam1.wsserver;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.Collections;

@SpringBootApplication
public class WebServiceApplication {

    public static void main(String[] args) {
       // SpringApplication.run(WebServiceApplication.class, args);
        SpringApplication app = new SpringApplication(WebServiceApplication.class);
        app.setDefaultProperties(Collections
                .singletonMap("server.port", "8183"));
        app.run(args);
    }

}
package cz.cvut.fit.niam1.messaging;

import cz.cvut.fit.niam1.pojo.Trip;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.annotation.JmsListener;

@SpringBootApplication
@EnableJms
public class TripConsumerApplication {

    private final Logger logger = LoggerFactory.getLogger(TripConsumerApplication.class);

    @JmsListener(destination = "queueTrips")
    public void readMessage(Trip message) throws InterruptedException {
        logger.info("Received confirmation message: {}", message.getDestination());
        Thread.sleep(2000);
    }

    public static void main(String[] args) {
        SpringApplication.run(TripConsumerApplication.class, args);
    }

}

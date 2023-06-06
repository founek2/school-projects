package cz.cvut.fit.niam1.messaging;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.annotation.JmsListener;
import cz.cvut.fit.niam1.pojo.Order;

@SpringBootApplication
@EnableJms
public class BookingConsumerApplication {

    private final Logger logger = LoggerFactory.getLogger(BookingConsumerApplication.class);

    @JmsListener(destination = "queueBookings")
    public void readMessage(Order message) {
        logger.info("Received booking: {}", message.getId());
    }

    public static void main(String[] args) {
        SpringApplication.run(BookingConsumerApplication.class, args);
    }

}

package cz.cvut.fit.niam1.messaging;

import cz.cvut.fit.niam1.pojo.Order;
import cz.cvut.fit.niam1.pojo.Trip;
import org.apache.activemq.command.ActiveMQQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.annotation.JmsListener;
import org.springframework.jms.core.JmsTemplate;

import javax.jms.Queue;

@SpringBootApplication
@EnableJms
public class OrderProcessorApplication {

    private final Logger logger = LoggerFactory.getLogger(OrderProcessorApplication.class);

    @Autowired
    private JmsTemplate jmsTemplate;

    @Autowired
    private Queue queueBookings;

    @Autowired
    private Queue queueTrips;

    @Bean
    public Queue queueBookings() {
        return new ActiveMQQueue("queueBookings");
    }

    @Bean
    public Queue queueTrips() {
        return new ActiveMQQueue("queueTrips");
    }

    @JmsListener(destination = "allOrdersQueue",  selector = "type = 'order'")
    public void readMessage(Order message) {
        logger.info("Received order message: {}", message);

        jmsTemplate.convertAndSend(queueBookings, message);
    }

    @JmsListener(destination = "allOrdersQueue", selector = "type = 'trip'")
    public void readMessage2(Trip message) {
        logger.info("Received trip message: {}", message);

        jmsTemplate.convertAndSend(queueTrips,message);
    }

    public static void main(String[] args) {
        SpringApplication.run(OrderProcessorApplication.class, args);
    }

}

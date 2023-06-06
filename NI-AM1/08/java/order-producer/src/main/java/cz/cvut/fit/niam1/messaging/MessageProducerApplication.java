package cz.cvut.fit.niam1.messaging;

import cz.cvut.fit.niam1.pojo.Order;
import cz.cvut.fit.niam1.pojo.Trip;
import org.apache.activemq.command.ActiveMQQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.jms.annotation.EnableJms;
import org.springframework.jms.core.JmsTemplate;

import javax.jms.Queue;
import java.time.LocalDateTime;

@SpringBootApplication
@EnableJms
public class MessageProducerApplication implements CommandLineRunner {

    private final Logger logger = LoggerFactory.getLogger(MessageProducerApplication.class);

    @Autowired
    private JmsTemplate jmsTemplate;

    @Autowired
    private Queue queue;

    @Bean
    public Queue queue() {
        return new ActiveMQQueue("allOrdersQueue");
    }

    public static void main(String[] args) {
        SpringApplication.run(MessageProducerApplication.class, args);
    }

    @Override
    public void run(String... args) throws Exception {
        while (true) {
            Thread.sleep(5000);
            LocalDateTime time = LocalDateTime.now();
            logger.info("Sending order to {} at {}", queue.getQueueName(), time);
            // jmsTemplate.convertAndSend(queue, "Message at " + time);
            jmsTemplate.convertAndSend(queue, new Order(1L, "content"),  m -> {
                m.setStringProperty("type", "order");
                return m;
            });

            Thread.sleep(5000);
            LocalDateTime time2 = LocalDateTime.now();
            logger.info("Sending trip to {} at {}", queue.getQueueName(), time2);
            // jmsTemplate.convertAndSend(queue, "Message at " + time);
            jmsTemplate.convertAndSend(queue, new Trip(1L, "destination"), m -> {
                m.setStringProperty("type", "trip");
                return m;
            });
        }
    }

}

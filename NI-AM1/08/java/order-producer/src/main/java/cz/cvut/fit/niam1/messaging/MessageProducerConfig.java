package cz.cvut.fit.niam1.messaging;

import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.region.policy.PolicyEntry;
import org.apache.activemq.broker.region.policy.PolicyMap;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

@Configuration
public class MessageProducerConfig {

    @Bean
    public BrokerService broker() throws Exception {
        BrokerService brokerService = new BrokerService();
        brokerService.addConnector("tcp://localhost:61616");
        brokerService.setPersistent(false);
        brokerService.setDestinationPolicy(policyMap());
        return brokerService;
    }

    @Bean
    public PolicyMap policyMap(){
        PolicyMap destinationPoliciy = new PolicyMap();
        List<PolicyEntry> entries = new ArrayList<PolicyEntry>();
        PolicyEntry queueEntry = new PolicyEntry();
        queueEntry.setQueue(">");
        queueEntry.setStrictOrderDispatch(false);
        entries.add(queueEntry);
        destinationPoliciy.setPolicyEntries(entries);
        return destinationPoliciy;
    }
}

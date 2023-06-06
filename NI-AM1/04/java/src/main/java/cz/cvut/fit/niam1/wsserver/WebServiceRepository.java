package cz.cvut.fit.niam1.wsserver;

import cz.cvut.fit.niam1.webservices.client.Payment;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;

@Component
public class WebServiceRepository {

    private static final List<Payment> payments = new ArrayList<>();

    @PostConstruct
    public void initRepo(){
        Payment p1 = new Payment();
        p1.setId(1);
        p1.setCardNumber("451167209823");
        p1.setCardOwner("Pepa Kopr");
        payments.add(p1);
    }

    public void addPayment(Payment p){
        //if (payments.stream().noneMatch(o -> o.getId() == p.getId() )) {
            payments.add(p);
        //}
    }

    public List<Payment> getPayments(){
        return payments;
    }
}

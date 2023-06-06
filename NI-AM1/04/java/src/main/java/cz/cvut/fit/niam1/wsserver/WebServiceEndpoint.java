package cz.cvut.fit.niam1.wsserver;

import cz.cvut.fit.niam1.webservices.client.*;
import cz.cvut.fit.niam1.wsserver.wsClient.WebServiceClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.server.endpoint.annotation.Endpoint;
import org.springframework.ws.server.endpoint.annotation.PayloadRoot;
import org.springframework.ws.server.endpoint.annotation.RequestPayload;
import org.springframework.ws.server.endpoint.annotation.ResponsePayload;

@Endpoint
public class WebServiceEndpoint {

    @Autowired
    private WebServiceRepository repository;

    @Autowired
    private WebServiceClient client;

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "getPaymentsRequest")
    @ResponsePayload
    public GetPaymentsResponse getPayments(@RequestPayload GetPaymentsRequest request) {
        GetPaymentsResponse response = new GetPaymentsResponse();
        response.getPayment().addAll(repository.getPayments());
        return response;
    }

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "addPaymentRequest")
    @ResponsePayload
    public AddPaymentResponse addPayment(@RequestPayload AddPaymentRequest request) {
        AddPaymentResponse response = new AddPaymentResponse();
        final Payment payment = request.getPayment();
        if (client.validateCard(payment)) {
            repository.addPayment(payment);
            response.setResult(true);
        } else response.setResult(false);

        return response;
    }
}

package cz.cvut.fit.niam1.wsserver.wsClient;

import cz.cvut.fit.niam1.webservices.client.Payment;
import cz.cvut.fit.niam1.webservices.client.ValidateCardRequest;
import cz.cvut.fit.niam1.webservices.client.ValidateCardResponse;
import org.springframework.ws.client.core.support.WebServiceGatewaySupport;

public class WebServiceClient extends WebServiceGatewaySupport {

    public boolean validateCard(Payment p) {
        ValidateCardRequest request = new ValidateCardRequest();
        request.setCardNumber(p.getCardNumber());
        request.setCardOwner(p.getCardOwner());
        ValidateCardResponse r =  (ValidateCardResponse) getWebServiceTemplate()
                .marshalSendAndReceive(request);

        return r.isResult();
    }

}

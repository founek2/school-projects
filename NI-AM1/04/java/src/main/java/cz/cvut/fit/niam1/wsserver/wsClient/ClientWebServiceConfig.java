package cz.cvut.fit.niam1.wsserver.wsClient;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;
import org.springframework.ws.config.annotation.EnableWs;
import org.springframework.ws.config.annotation.WsConfigurerAdapter;

@EnableWs
@Configuration
public class ClientWebServiceConfig extends WsConfigurerAdapter {
    @Bean
    public Jaxb2Marshaller marshaller() {
        Jaxb2Marshaller marshaller = new Jaxb2Marshaller();
        marshaller.setContextPath("cz.cvut.fit.niam1.webservices.client");
        return marshaller;
    }

    @Bean
    public WebServiceClient wsClient(Jaxb2Marshaller marshaller) {
        WebServiceClient client = new WebServiceClient();
        client.setDefaultUri("http://147.32.233.18:8888/NI-AM1-CardValidation/ws");
        client.setMarshaller(marshaller);
        client.setUnmarshaller(marshaller);
        return client;
    }
}

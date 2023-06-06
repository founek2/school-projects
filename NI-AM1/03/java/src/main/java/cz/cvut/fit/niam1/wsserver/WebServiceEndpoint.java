package cz.cvut.fit.niam1.wsserver;

import https.courses_fit_cvut_cz.ni_am1.tutorials.web_services.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.server.endpoint.annotation.Endpoint;
import org.springframework.ws.server.endpoint.annotation.PayloadRoot;
import org.springframework.ws.server.endpoint.annotation.RequestPayload;
import org.springframework.ws.server.endpoint.annotation.ResponsePayload;

import java.util.Optional;

@Endpoint
public class WebServiceEndpoint {

    @Autowired
    private WebServiceRepository repository;

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "getBookingsRequest")
    @ResponsePayload
    public GetBookingsResponse getTours(@RequestPayload GetBookingsRequest request) {
        GetBookingsResponse response = new GetBookingsResponse();
        response.getBooking().addAll(repository.getBookings());
        return response;
    }

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "getBookingRequest")
    @ResponsePayload
    public GetBookingResponse deleteBooking(@RequestPayload GetBookingRequest request) {
        GetBookingResponse response = new GetBookingResponse();
        Optional<Booking> booking = repository.getBooking(request.getId());

        booking.ifPresent(response::setBooking);

        return response;
    }

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "deleteBookingRequest")
    @ResponsePayload
    public DeleteBookingResponse deleteBooking(@RequestPayload DeleteBookingRequest request) {
        DeleteBookingResponse response = new DeleteBookingResponse();
        repository.deleteBooking(request.getId());
        return response;
    }

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "createBookingRequest")
    @ResponsePayload
    public GetBookingResponse createBooking(@RequestPayload CreateBookingRequest request) {
        GetBookingResponse response = new GetBookingResponse();
        Booking b = repository.addBooking(request.getBooking());
        response.setBooking(b);
        return response;
    }

    @PayloadRoot(namespace = "https://courses.fit.cvut.cz/NI-AM1/tutorials/web-services/", localPart = "updateBookingRequest")
    @ResponsePayload
    public GetBookingResponse updateBooking(@RequestPayload UpdateBookingRequest request) {
        GetBookingResponse response = new GetBookingResponse();
        Optional<Booking> b = repository.updateBooking(request.getBooking());
        b.ifPresent(response::setBooking);
        return response;
    }
}

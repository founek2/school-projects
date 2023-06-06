package cz.cvut.fit.niam1.wsserver;

import https.courses_fit_cvut_cz.ni_am1.tutorials.web_services.*;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.util.*;

@Component
public class WebServiceRepository {

    private static final List<Booking> bookings = new ArrayList<>();
    private static long sequence = 0;

    @PostConstruct
    public void initRepo() throws DatatypeConfigurationException {
        Booking b1 = new Booking();
        Date current_date = new Date();

        Arrival a = new Arrival();
        a.setTime(dateToXmlCalendar(current_date));
        a.setAirportId(AirportIdentifier.ID_000);
        b1.setArrival(a);

        Departure d = new Departure();
        d.setTime(dateToXmlCalendar(current_date));
        d.setAirportId(AirportIdentifier.ID_123);
        b1.setDeparture(d);

        Passenger p1 = new Passenger();
        p1.setName("Alfonz");
        p1.setSurname("Starý");
        b1.setPassenger(p1);

        addBooking(b1);
    }

    public Booking addBooking(Booking b){
        bookings.add(b);
        return b;
    }

    private Booking merge(Booking b2, BookingCreate b1){
        b2.setPassenger(b1.getPassenger());
        b2.setDeparture(b1.getDeparture());
        b2.setArrival(b1.getArrival());
        return b2;
    }

    private Booking merge(Booking b2, Booking b1){
        b2.setPassenger(b1.getPassenger());
        b2.setDeparture(b1.getDeparture());
        b2.setArrival(b1.getArrival());
        return b2;
    }

    public Booking addBooking(BookingCreate create){
        Booking b = new Booking();
        b.setId(sequence++);
        merge(b, create);

        bookings.add(b);
        return b;
    }

    public List<Booking> getBookings(){
        return bookings;
    }

    public void deleteBooking(long id){
        bookings.removeIf(t -> t.getId() == id);
    }

    public Optional<Booking> getBooking(long id) {
        return bookings.stream()
                .filter(booking -> booking.getId() == id)
                .findAny();
    }

    public Optional<Booking> updateBooking(Booking b ) {
        Optional<Booking> booking = getBooking(b.getId());

        return booking.map(book -> {
            merge(book, b);
            return book;
        });
    }

    private XMLGregorianCalendar dateToXmlCalendar(Date date) throws DatatypeConfigurationException {
        GregorianCalendar gc = new GregorianCalendar();
        gc.setTime(date);
        return DatatypeFactory.newInstance().newXMLGregorianCalendar(gc);
    }
}

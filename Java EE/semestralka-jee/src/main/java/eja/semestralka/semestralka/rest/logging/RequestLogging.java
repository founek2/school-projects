package eja.semestralka.semestralka.rest.logging;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.ext.Provider;
import java.util.logging.Logger;

@Logged
@Provider
public class RequestLogging implements ContainerRequestFilter {
    private static final Logger LOG = Logger.getLogger(RequestLogging.class.getName());

    @Override
    public void filter(ContainerRequestContext requestContext) {
        LOG.info(requestContext.getMethod() + " " + requestContext.getUriInfo().getPath());
    }
}

package eja.semestralka.semestralka.rest;

import eja.semestralka.semestralka.rest.logging.Logged;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

@Logged
@ApplicationPath("api")
public class RestApplication extends Application {

}

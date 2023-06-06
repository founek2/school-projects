#!/bin/bash

tarql --dedup 500000 ./cities.sparql ../datasets/worldcities.csv > ../output/cities.ttl
tarql --dedup 500000 ./airports.sparql ../datasets/airports_header.csv > ../output/airports.ttl
tarql --dedup 500000 ./fligh_records.sparql ../datasets/flightlist_1000.csv  > ../output/flight_records.ttl
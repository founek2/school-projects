= HW5 - TBD!

# Restful API design

country (1) ← (0..N) location (1) ← (0..N) tour (0..N) → (0..N) customer

type `Link<T>` = {"href": "base_url/{T}", rel: "list" | "add" | "remove" } | {"href": "base_url/{T}/{id}", rel: "self" | "remove" }

`/tour`

-   GET - retrieve all
    -   Status codes: 200
    -   Returns: {tour: {..., \_links: Link<customer | location>[]}}[]
-   POST - create
    -   Status codes: 201, 400
-   PUT - replace, can be used as delete with empty list in body

`/tour/{id}`

-   GET - retrieve
    -   Status codes: 200, 404
    -   Returns: {tour: {...}, \_links: Link<customer | location>[]}
-   PUT - replace
    -   Status codes: 204, 400, 404
-   DELETE - delete
    -   Status codes: 204, 400, 404

`/customer`

-   GET - retrieve all
    -   Status codes: 200
    -   Returns: {customer: {..., \_links: Link<tour>[]}}[]
-   POST - create
    -   Status codes: 201, 400
-   PUT - replace, can be used as delete with empty list in body

`/customer/{id}`

-   GET - retrieve
    -   Status codes: 200, 404
    -   Returns: {customer: {...}, \_links: Link<tour>[]}
-   PUT - replace
    -   Status codes: 204, 400, 404
-   DELETE - delete
    -   Status codes: 204, 400, 404

`/location`

-   GET - retrieve all
    -   Status codes: 200
    -   Returns: {location: {..., \_links: Link<tour | country>[]}}[]
-   POST - create
    -   Status codes: 201, 400
-   PUT - replace, can be used as delete with empty list in body

`/location/{id}`

-   GET - retrieve
    -   Status codes: 200, 404
    -   Returns: {location: {...}, \_links: Link<tour | country>[]}
-   PUT - replace
    -   Status codes: 204, 400, 404
-   DELETE - delete
    -   Status codes: 204, 400, 404

`/country`

-   GET
    -   query parameters:
        -   search - full text search over text representation
        -   climate - ENUM filter: Tropical, Dry, Temperate, Continental, Polar
    -   Status codes: 200
    -   Returns: {location: {..., \_links: Link<location>[]}}[]
-   POST - create
    -   Status codes: 201, 400
-   PUT - replace, can be used as delete with empty list in body

`/country/{id}`

-   GET - retrieve
    -   Status codes: 200, 404
    -   Returns: {location: {...}, \_links: Link<location>[]}
-   PUT - replace
    -   Status codes: 204, 400, 404
-   DELETE - delete
    -   Status codes: 204, 400, 404

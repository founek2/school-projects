= HW7 - TBD!

Pro implementaci Etag jsem využil `ShallowEtagHeaderFilter`, která řeší vše za mě. Pro implementaci Weak Etag jsem musel řešit implementaci ručně s výpočtem Etag a porovnáním.

-   /tour - využívá Etag
-   /tour2 - využívá weak Etag

![Get without etag](screenshots/getWithoutEtag.png)
Get without etag

![Get with etag](screenshots/getWithEtag.png)
Get with etag

![Get without weak](screenshots/getWithoutWeak.png)
Get without weak etag

![Get with weak](screenshots/getWithWeak.png)
Get with weak etag

![Add tour](screenshots/addTour.png)
Add tour

![getWithEtagOnModified](screenshots/getWithEtagOnModified.png)
![headers](screenshots/headers.png)
Get with etag on modified

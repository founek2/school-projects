## TODO

-   pageview binary
-   duration on last page -> as mean of all previos duration

! transaction is one user visit

### User-pageview matrix UPM

     pageA pageB pageC ...

t1 1 0 10
.
.
.

### Pageview-feature matrix (PFM)

        category1 category2 ...

pageA 1 0
pageB 0 1
pageC 1 1

### Content-enhanced transaction matrix (TFM)

TFM = UPM × PFM

### Data modeling

transaction order URL Duration

### Cleaning data

### Apriori

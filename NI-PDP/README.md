# NI-PDP

|               | 10_7 | 12_6 | 15_5 |
| ------------- | ---- | ---- | ---- |
| Sequential    | 4,2  | 4,2  | 6,1  |
| Task parallel | 1,6  | 5,1  | 3,9  |
| Data parallel | 1,3  | 1,1  | 1,8  |
| mpi parallel  | 1,3  | 3    | 3    |

> measured (unit seconds) running in Docker with 4 cores assigned, HW: Mac Air M1

## Sériový

| stroj   | 15_6 | 15_6_2 | 18_5  | 14_7 | 15_6_3 | 15_6_4 | 22_4 | 14_6 | 18_5_2 | 14_6_2 |
| ------- | ---- | ------ | ----- | ---- | ------ | ------ | ---- | ---- | ------ | ------ |
| ryzen   | 166  | 219    | 232   | 853  | 717    | 451    | 370  | 45   | 121    | 113    |
| cluster | 391  |        | \*111 | 287  | 268    |

> measured (unit seconds) running in Docker with 6 cores assigned, HW: AMD Ryzen 3600

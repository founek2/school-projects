# HW5 - TBD!

| Resource                       | Format | GET                  | DELETE         |
| ------------------------------ | ------ | -------------------- | -------------- |
| http://company.at/customers    | XML    | XMLHttpRequest       | XMLHttpRequest |
| http://company.at/suppliers    | JSON   | XMLHttpRequest,JSONP | XMLHttpRequest |
| http://weather.at/innsbruck    | XML    | CORS                 | CORS           |
| http://people.at/students      | JSON   | CORS,JSONP           | CORS           |
| http://people.at/{dob}/contact | VCARD  | CORS                 | CORS           |

> Některé HTTP requesty nevyžadují CORS preflight mimo origin, ale pouze v případě pokud se jedná o GET | HEAD | POST a Content-Type je application/x-www-form-urlencoded | multipart/form-data | text/plain - což dle formátu nebude ani v jednom případě.

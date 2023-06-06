= HW2 - TBD!

```bash
telnet 147.32.233.18 8888

GET /NI-AM1-ApplicationProtocols/httpTelnet1 HTTP/1.0
Host: 147.32.233.18:8888
User-Agent: fit-telnet
Accept: text/html; charset=UTF-8
Accept-Language: en-US

```

Z mne neznámého důvodu, API stále vracelo chybu: `Fail, use utf-8!`.

```bash
telnet 147.32.233.18 8888

POST /NI-AM1-ApplicationProtocols/httpTelnet2 HTTP/1.0
Host: 147.32.233.18:8888
Referer: mi-mdw
Content-type: application/x-www-form-urlencoded
Content-length: 8

data=fit
```

```bash
curl http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/welcom
# OK
# Your next page is /protocol/get
# send GET parameter "name" with value "apr"
# set Header "Accept" to "text/plain"


curl --header "Accept: text/plain" http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/get?name=apr
# OK
# Your next page is /protocol/post
# send POST parameter "name" with value "benefit"
# and set Header "Accept" is "text/plain"
# and set Header "Accept-Language" is "en-US"

curl -X POST --header "Accept: text/plain" --header "Accept-Language: en-US" --data "name=benefit" http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/post
# OK
# Your next page is /protocol/referer
# change referer to value "processor"
# set Header "Accept" is "text/html"

curl --header "Referer: processor" --header "Accept: text/html" http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/referer
# OK
# Your next page is /protocol/useragent
# and change User-Agent to value "firstly"
# and set Header "Accept-Language" is "en-US"

curl --header "User-Agent: firstly" --header "Accept-Language: en-US" http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/useragent
# OK
# Your next page is /protocol/cookie
# send cookie called "name" with value "increase"


curl --header "Cookie: name=increase"  http://147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/cookie
# OK
# Your next page is /protocol/auth
# authenticate by "zur:homosexuality"
# set Header "Accept" is "text/html"

curl --header "Accept: text/html"  http://zur:homosexuality@147.32.233.18:8888/NI-AM1-ApplicationProtocols/protocol/auth
# OK
# Your final result is: pressed
```

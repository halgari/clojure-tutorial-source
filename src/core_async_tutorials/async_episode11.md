Core.Async Episode 11 - Distributed Programming via Client/Server

* HTTP
* 0MQ


* Client uploading a message to Server
* Upload - 200 back is okay anything else is not



1) make connection upload data
2) connection resets
3) ???
4) failure...
5) don't commit if connection resets


Sent data could be anywhere
1) local unflushed buffer
2) server's network stack
3) router's stack


* At-least-once delivery

What is the problem with duplicate data?

1) bad things happen (e.g. money transfers)
2) You may not care, it can be de-dupped
3) You may not care if duplicates exist (how often do duplicates occur?)


Pros of this method:

1) It's pretty simple
2) HTTP exists - may be the only option.
3) Often a one-way connection
3.1) HTTP we have SSE & Websockets
3.2) Long polling
3.3) Everything is a server?
4) existing tech (hw and sw)
4.1) load balancers
4.2) cache servers
4.3) compressors
4.4) debuggers
5) interop with other systems






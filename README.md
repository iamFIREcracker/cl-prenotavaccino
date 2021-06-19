# prenotavaccino
Web scraping for fun and profit^W for a registration to get my COVID-19 jab.

From the original [write-up](https://matteolandi.net/plan.html#day-2021-06-02):

> Rumor says that in a few days everyone in Tuscany, irrespective of their age,
> could finally register for their first shot of the vaccine; that means "click
> day", i.e. continuously polling the registration website until your
> registration category is open.  I am sure a bird will eventually announce
> when the service will be open, i.e. the time of the day, so no need to stay
> on the lookout for the whole day, but I figured I could use this as an excuse
> to do a little bit of Web scraping in Common Lisp, so here it goes.

## Instructions

Clone the repository:

    $ cd ~/quicklisp/local-projects
    $ git clone https://github.com/iamFIREcracker/cl-prenotavaccino.git

Quickload the defined system:

    > (ql:quickload "prenotavaccino")
    To load "prenotavaccino":
      Load 1 ASDF system:
        prenotavaccino
    ; Loading "prenotavaccino"
    .....
    ("prenotavaccino")

Call PRENOTAVACCINO:START:

    > (prenotavaccino:start)

Note: the scraper uses Twilio to send notification SMSs when "something
changed"; the following environment variables can be used to provide your
Twilio's account details:

- `TWILIO_ACCOUNT_SID`: your account SID
- `TWILIO_AUTH_TOKEN`: the authentication token
- `TWILIO_FROM_NUMBER`: the _from_ number of the messaging service
- `TWILIO_TO_NUMBERS`: who to send messages to (space separated list)

If any of these environment variables is found empty, you will be prompted to
provide a value as soon as START is invoked:

    > (prenotavaccino:start)
    TWILIO_ACCOUNT_SID=qwerasdfzxcv
    TWILIO_AUTH_TOKEN=qwerasdfzxcv
    TWILIO_FROM_NUMBER=+12345678
    TWILIO_TO_NUMBERS=+23456789 +345678901

To stop the scraper simply call the STOP function:

    > (prenotavaccino:stop)

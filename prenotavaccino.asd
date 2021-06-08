(asdf:defsystem #:prenotavaccino
  :description "Web scraping for fun and profit^W for a registration to get my COVID-19 jab"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:bordeaux-threads
                 #:drakma
                 #:local-time
                 #:st-json
                 #:uiop
              )

  :components ((:file "main")))

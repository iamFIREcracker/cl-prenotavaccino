(uiop:define-package #:prenotavaccino
  (:export run))
(in-package :prenotavaccino)

(defun main ()
  (loop
    (with-simple-restart (continue "Ignore the error")
      (wait-for-active-categories)
      (send-sms))))


(defvar *last-run-at* nil "When the categories scraper last run")

(defun wait-for-active-categories ()
  (loop :when (categories-activated-p) :return it
        :do (progn
              (setf *last-run-at* (local-time:now))
              (random-sleep))))


(defvar *categories-snapshots* nil
  "List of categories snapshots -- handy to see what changed over time.")

(defun categories-activated-p ()
  (let ((categories (fetch-categories)))
    (unless *categories-snapshots*
      (push categories *categories-snapshots*))
    (prog1 (find-recently-activated categories)
      (push categories *categories-snapshots*)
      (harvest-categories-snapshots))))


(define-condition html-not-json-content-type () ())

(defparameter *prenotavaccino-home-url* "https://prenotavaccino.sanita.toscana.it")
(defparameter *prenotavaccino-categories-url* "https://prenotavaccino.sanita.toscana.it/api/index")
(defvar *cookie-jar* (make-instance 'drakma:cookie-jar)
  "Cookie jar to hold session cookies when interacting with Prenotavaccino")

(defun parse-json (response)
  (let* ((string (flexi-streams:octets-to-string response)))
    (st-json:read-json string)))

(defun fetch-categories ()
  (flet ((fetch-it ()
           (multiple-value-bind (response status headers)
               (drakma:http-request *prenotavaccino-categories-url* :cookie-jar *cookie-jar*)
             (declare (ignore status))
             (let ((content-type (cdr (assoc :content-type headers))))
               (if (equal content-type "text/html")
                   (error 'html-not-json-content-type)
                 response))))
         (parse-it (response)
           (st-json:getjso "categories" (parse-json response))))
    (parse-it
      (handler-case (fetch-it)
        (html-not-json-content-type (c)
          (declare (ignore c))
          (format t "~&Received HTML instead of JSON. Retrying assuming the previous session expired...")
          (fetch-it))))))


(defun category-name (cat) (st-json:getjso "idCategory" cat))
(defun category-title (cat) (st-json:getjso "title" cat))
(defun category-active-p (cat)
  (and (eql (st-json:getjso "active" cat) :true)
       (eql (st-json:getjso "forceDisabled" cat) :false)))

(defun find-recently-activated (categories)
  (flet ((category-by-name (name categories)
           (find name categories :key #'category-name :test #'equal)))
    (let ((prev-categories (first *categories-snapshots*)))
      (loop :for cat :in categories
            :for cat-prev = (category-by-name (category-name cat) prev-categories)
            :when (and (category-active-p cat)
                       (or (not cat-prev)
                           (not (category-active-p cat-prev))
                           (not (equal (category-title cat-prev) (category-title cat)))))
            :collect cat))))


(defparameter *categories-snapshots-max-length* 50
  "Maximum numbers of categories snapshots to keep around")

;; could have removed "consecutive duplicates" from the list, or
;; prevented these from getting stored in the list to begin with...
(defun harvest-categories-snapshots ()
  (when (> (length *categories-snapshots*) *categories-snapshots-max-length*)
    (setf *categories-snapshots*
          (subseq *categories-snapshots* 0 (1+ *categories-snapshots-max-length*)))))


(defparameter *sleep-seconds-min* 60
  "Minimum number of seconds the scraper will sleep for")
(defparameter *sleep-seconds-jitter* 5
  "Adds a pinch of randomicity -- see RANDOM-SLEEP")

(defun random-sleep ()
  (sleep (+ *sleep-seconds-min* (random *sleep-seconds-jitter*))))


(defparameter *twilio-messages-api-url-template* "https://api.twilio.com/2010-04-01/Accounts/~a/Messages.json")
(defvar *twilio-account-sid* (uiop:getenv "TWILIO_ACCOUNT_SID"))
(defvar *twilio-auth-token* (uiop:getenv "TWILIO_AUTH_TOKEN"))
(defvar *twilio-from* (uiop:getenv "TWILIO_FROM_NUMBER") "Twilio's FROM number")
(defvar *twilio-to* (uiop:getenv "TWILIO_TO_NUMBERS") "Space spearated list of numbers to send SMSs to")

(defun send-sms (&optional (body (sms-body)))
  (flet ((send-it (to)
           (drakma:http-request (format nil *twilio-messages-api-url-template* *twilio-account-sid*)
                                :method :post
                                :basic-authorization `(,*twilio-account-sid* ,*twilio-auth-token*)
                                :parameters `(("Body" . ,body)
                                              ("From" . ,*twilio-from*)
                                              ("To" . ,to)))))
    (let (failed)
      (dolist (to (split-sequence:split-sequence #\Space *twilio-to*))
        (let* ((jso (parse-json (send-it to)))
               (error-code (st-json:getjso "error_code" jso)))
          (unless (eql error-code :null)
            (push jso failed))))
      (if failed
          (error "Failed to deliver **all** SMSs: ~a" failed)
          t))))

(defun sms-body ()
  (let* ((categories (first *categories-snapshots*))
         (*categories-snapshots* (cdr *categories-snapshots*)))
    (format nil "Ora attivi: ~{~A~^, ~} -- ~a"
            (mapcar #'category-title
                    (find-recently-activated categories))
            *prenotavaccino-home-url*)))


(defun getenv-or-readline (name)
  "Gets the value of the environment variable, or asks the user to provide
  a value for it."
  (or (uiop:getenv name)
      (progn 
        (format *query-io* "~a=" name)
        (force-output *query-io*)
        (read-line *query-io*))))

(defun run ()
  (let ((*twilio-account-sid* (getenv-or-readline "TWILIO_ACCOUNT_SID"))
        (*twilio-auth-token* (getenv-or-readline "TWILIO_AUTH_TOKEN"))
        (*twilio-from* (getenv-or-readline "TWILIO_FROM_NUMBER"))
        (*twilio-to* (getenv-or-readline "TWILIO_TO_NUMBERS")))
    (main)))

#; Scratch
(in-package :prenotavaccino)

(setf drakma:*header-stream* t)

(fetch-categories)
(categories-activated-p)

(sms-body)
(send-sms)

(find "LastMinute" (first *categories-snapshots*)
      :key #'category-name
      :test #'string=)

(loop for snap in *categories-snapshots*
  do (loop for cat in snap
       when (equal (category-name cat) "ByAge20")
       do (pr cat)))

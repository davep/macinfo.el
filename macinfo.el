;;; Work in progress!

(require 'url)
(require 'cl-lib)

;; Information sources:
;; https://www.finetunedmac.com/forums/ubbthreads.php?ubb=showflat&Number=32011
;; http://blog.coriolis.ch/2011/08/01/get-your-apple-device-model-name-in-a-readable-format/

(defun macinfo-decode-sn-11 (sn)
  "Decode the content of 11 digit serial number SN."
  (error "11 digit serial numbers aren't handled yet"))

(defun macinfo-decode-sn-12-year-decoder (year)
  "Decode YEAR from a serial number into something more useful.

The return value is a `cons' where the `car' is t or nil to show
if the hardware was manufactured early (t) or late (nil) in the
year; the `cdr' is the year."
  (let* ((lookup "123456789CDFGHJKLMNPQRTVWX")
         (match  (cl-search year lookup)))
    (when match
      (cons (zerop (mod match 2)) (+ 2005 (truncate (/ match 2)))))))

(defun macinfo-decode-sn-12-year (year)
  "Decode and describe YEAR from a serial number."
  (let ((decoded (macinfo-decode-sn-12-year-decoder year)))
    (when decoded
      (format "%s %d" (if (car decoded) "Early" "Late") (cdr decoded)))))

(defun macinfo-decode-sn-12-week (week year)
  "Decode the week of manufacture from a serial number."
  (let* ((lookup "0123456789CDFGHJKLMNPQRTVWX")
         (match  (cl-search week lookup))
         (year   (macinfo-decode-sn-12-year-decoder year)))
    (when match
      (+ match (if (car year) 1 28)))))

(defun macinfo-decode-sn-12-model-code (mc)
  (with-current-buffer
      (url-retrieve-synchronously (format "http://support-sp.apple.com/sp/product?cc=%s&lang=en_US" mc) t)
    (setf (point) (point-min))
    (when (search-forward-regexp "<configCode>\\(.*\\)</configCode>")
      (match-string 1))))

(defun macinfo-decode-sn-12 (sn)
  "Decode the content of 12 digit serial number SN."
  (list
   (cons 'plant-code          (substring sn 0 3))
   (cons 'year-of-manufacture (macinfo-decode-sn-12-year (substring sn 3 4)))
   (cons 'week-of-manufacture (macinfo-decode-sn-12-week (substring sn 4 5) (substring sn 3 4)))
   (cons 'unique-unit-code    (substring sn 5 8))
   (cons 'model-code          (macinfo-decode-sn-12-model-code (substring sn 8 12)))))

(defconst macinfo-decoders
  '((11 . macinfo-decode-sn-11)
    (12 . macinfo-decode-sn-12))
  "Association list of serial number decoders.")

(defun macinfo-get-serial-number ()
  "Get the serial number."
  (with-temp-buffer
    (let ((sysprof "system_profiler"))
      (when (executable-find sysprof)
        (save-excursion
          (call-process sysprof nil t nil "SPHardwareDataType"))
        (when (search-forward "Serial Number (system): ")
          (buffer-substring-no-properties (point) (point-at-eol)))))))

;;;###autoload
(defun macinfo-serial-number ()
  "Show the serial number of a Mac."
  (interactive)
  (let ((sn (macinfo-get-serial-number)))
    (if sn
        (message "Serial number: %s" sn)
      (message "Unable to get the serial number, or this isn't a Mac."))))

(defun macinfo-show-decoded-serial-number (sn)
  "Decode and show the information in serial number SN."
  (let ((decoder (cdr (assoc (length sn) macinfo-decoders)))
        (help-window-select t))
    (with-help-window "*macinfo*"
      (if decoder
          (princ (funcall decoder sn))
        (princ "Sorry, I don't know how to decode %s" sn)))))

;;;###autoload
(defun macinfo ()
  "Show as much information as we can gather from a Mac's serial number."
  (interactive)
  (let ((sn (macinfo-get-serial-number)))
    (if sn
        (macinfo-show-decoded-serial-number sn)
      (error "Unable to get the serial number, or this isn't a Mac."))))

(provide 'macinfo)

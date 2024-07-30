
;;  ██████╗███████╗███████╗██╗      ██████╗ ███████╗███████╗██╗███╗   ██╗███████╗
;; ██╔════╝██╔════╝██╔════╝██║      ██╔══██╗██╔════╝██╔════╝██║████╗  ██║██╔════╝
;; ██║     █████╗  █████╗  ██║█████╗██║  ██║█████╗  █████╗  ██║██╔██╗ ██║█████╗
;; ██║     ██╔══╝  ██╔══╝  ██║╚════╝██║  ██║██╔══╝  ██╔══╝  ██║██║╚██╗██║██╔══╝
;; ╚██████╗██║     ██║     ██║      ██████╔╝███████╗██║     ██║██║ ╚████║███████╗
;;  ╚═════╝╚═╝     ╚═╝     ╚═╝      ╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═══╝╚══════╝
;; TODO: Add a crash scenario when the value is not number parsable
;; Might want to collect all offenders and then crash at the end
(in-package :cffi-define)

(defmacro defcenum! (name-and-options headers &body enum-list)
  "Using passed headers, compile and read out define values for the enum-list and produce a cffi:defcenum macro call"
  (let* ((defines (read-defines headers (mapcar #'car enum-list)))
	 (faulty-defines (remove-if-not (lambda (item) item) (mapcar #'is-define-faulty defines))))
    (when faulty-defines
      (error (format nil "Faulty defines: ~a" faulty-defines)))
    `(cffi:defcenum ,name-and-options
       ,@(loop for (c-key key) in enum-list
	       collect (list key (parse-integer (cadr (car (member (string c-key) defines :test 'equal :key 'car)))))))))

(defun read-defines (headers defines)
  "Given a list of headers and defines - produces a temporary c file
with said headers included, and said defines being printed out via pragma message"
  (uiop:with-temporary-file (:stream stream :pathname file :prefix "" :suffix ".c")
    (loop for header in headers do (format stream "#include ~a~%" header))
    (format stream "#define XSTR(x) STR(x)~%")
    (format stream "#define STR(x) #x~%")
    (loop for define in defines do (format stream "#pragma message \"~a: \" XSTR(~a)~%" define define))
    (finish-output stream)

    (multiple-value-bind (_ old-name file)
	(rename-file (namestring file) (format nil "~a.c" (namestring file)))
      (declare (ignore _))
      (unwind-protect
	   (let* ((output-file (format nil "~a-delete" (namestring file)))
		  (process (uiop:launch-program (list "/usr/bin/env" "gcc" "-shared"
						      "-o" output-file
						      "-Werror" "-fPIC"
						      (namestring file))
						:output :stream
						:error-output :stream))
		  (exit-code (uiop:wait-process process)))

	     ;; TODO: You could also extract more concrete error information here
	     (when (plusp exit-code)
	       (print (uiop/stream:slurp-stream-string (uiop:process-info-error-output process)))
	       (error "Failed to compile defcenum! c temporary file. See log for details."))

	     (prog1
		 (loop for line in (uiop/stream:slurp-stream-lines (uiop:process-info-error-output process))
		       for result = (cl-ppcre:register-groups-bind (key value)
					("#pragma message: (.+): (.+)[’']" line)
				      (list key value))
		       when result collect result)

	       (delete-file output-file)))
	(rename-file file old-name)))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun is-number? (string)
  (handler-case (parse-integer string)
    (error () nil)))

(defun is-define-faulty (define)
  "Check if a define value is faulty"
  (let ((key (car define)) (value (cadr define)))
    (cond
      ((equal key value) (list key :missing)) ;; This most likely means that a define couldn't be found
      ((not (is-number? value)) (list key :not-numeric))
      (t nil))))

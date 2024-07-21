
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
  (let ((defines (read-defines headers (mapcar #'car enum-list))))
    `(cffi:defcenum ,name-and-options
       ,@(loop for (c-key key) in enum-list
	       collect (list key (parse-integer (cadr (car (print (member (print (string c-key)) (print defines) :test 'equal :key 'car))))))))))

(defun read-defines (headers defines)
  (uiop:with-temporary-file (:stream stream :pathname file :prefix "" :suffix ".c")
    (loop for header in headers do (format stream "#include ~a~%" header))
    (format stream "#define XSTR(x) STR(x)~%")
    (format stream "#define STR(x) #x~%")
    (loop for define in defines do (format stream "#pragma message \"~a: \" XSTR(~a)~%" define define))
    (finish-output stream)

    (multiple-value-bind (_ old-name file)
	(rename-file (namestring file) (format nil "~a.c" (namestring file)))
      (declare (ignore _))
      (let* ((output-file (format nil "~a-delete" (namestring file)))
	     (process (uiop:launch-program (list "/usr/bin/env" "gcc" "-shared"
						 "-o" output-file
						 "-Werror" "-fPIC"
						 (namestring file))
					   :output :stream
					   :error-output :stream)))
	(prog1
	    (loop for line in (uiop/stream:slurp-stream-lines (uiop:process-info-error-output process))
		  for result = (cl-ppcre:register-groups-bind (key value)
			      ("#pragma message: (.+): (.+)’" line)
				 (list key value))
		  when result collect result)
	  (rename-file file old-name)
	  (delete-file output-file))))))

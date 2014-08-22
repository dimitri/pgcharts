(in-package #:pgcharts)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct command verbs bindings help lambda)

  (defvar *commands* (make-array 0
                                 :element-type 'command
                                 :adjustable t
                                 :fill-pointer t)
    "Host commands defined with the DEFINE-COMMAND macro.")

  (defmethod same-command ((a command) (b command))
    "Return non-nil when a and b are commands with the same verbs"
    (equal (command-verbs a) (command-verbs b))))

(defun destructuring-match (lambda-list args)
  "Return non-nil when ARGS are matching against the given LAMBDA-LIST."
  (ignore-errors
    (funcall
     (compile nil
              `(lambda ()
                 ;; hide a style warning that variables are defined
                 ;; but never used here
                 (declare #+sbcl (sb-ext:muffle-conditions style-warning))
                 (destructuring-bind ,lambda-list ',args t))))))

(defmethod command-matches ((command command) args)
  "When the given COMMAND matches given command line ARGS, then return it
   and the argument to apply to it."
  (declare (type list args))
  (when (<= (length (command-verbs command)) (length args))
    (let ((matches-p (loop :for verb :in (command-verbs command)
                        :for arg in args
                        :for matches-p := (string-equal verb arg)
                        :while matches-p
                        :finally (return matches-p))))
      (when matches-p
        (let ((fun-args (nthcdr (length (command-verbs command)) args)))
          (when (destructuring-match (command-bindings command) fun-args)
            (list (command-lambda command) fun-args)))))))

(defmacro define-command ((verbs bindings) help-string &body body)
  "Define a command that is to be fired when VERBS are found at the
   beginning of the command, assigning remaining arguments to given
   bindings.

   The help-string is used when displaying the program usage."
  (let ((fun      (gensym))
        (command  (gensym))
        (position (gensym))
        (output   (gensym)))
   `(eval-when (:load-toplevel :compile-toplevel :execute)
      (let* ((,fun      (lambda ,bindings
                          (read-config)
                          (let ((,output (progn ,@body)))
                            (typecase ,output
                              (string (format t "~a~%" ,output))
                              (t      nil)))))
             (,command  (make-command :verbs ',verbs
                                      :bindings ',bindings
                                      :help ,help-string
                                      :lambda (compile nil ,fun)))
             (,position (position-if (lambda (c) (same-command c ,command))
                                     *commands*)))
        (if ,position
            (setf (aref *commands* ,position) ,command)
            (vector-push-extend ,command *commands*))))))

(defstruct (option
             (:conc-name opt-)
             (:constructor make-option (keyword short long
                                                &optional fun eat-next-arg)))
  keyword short long fun eat-next-arg)

(defun parse-option-name (arg)
  "When ARG is an option name, return its keyword, otherwise return nil."
  (loop :for option :in *options*
     :when (or (string= arg (opt-short option))
               (string= arg (opt-long option)))
     :return option))

(defun process-argv-options (argv)
  "Return the real args found in argv, and a list of the options used, as
  multiple values."
  (let ((args   '())
        (ignore nil)
        (opts   '()))
    (values (loop :for (arg next) :on (rest argv)
               :for opt := (unless ignore (parse-option-name arg))
               :do (progn
                     ;; sanity check
                     (when (and opt (opt-eat-next-arg opt) (null next))
                       (format t "Missing argument for option ~a~%" arg)
                       (push :help opts))

                     ;; build the argument list
                     (unless (or opt ignore)
                       (push arg args))

                     ;; we might have to ignore arg on next iterationa
                     (setf ignore (and opt (opt-eat-next-arg opt)))

                     ;; deal with the option side effects
                     (when opt
                       (push (opt-keyword opt) opts)
                       (when (opt-fun opt)
                         (let ((args (when (opt-eat-next-arg opt) (list next))))
                           (apply (opt-fun opt) args)))))
               :finally (return (nreverse args)))
            opts)))

(defun find-command-function (args)
  "Loop through *COMMANDS* to find the code to execute given ARGS."
  (loop :for command :across *commands*
     :for match := (command-matches command args)
     :until match
     :finally (return match)))

(defun usage (args &key help)
  "Loop over all the commands and output the usage of the main program"
  (format t "pgcharts [ --help ] [ --version ] [ --config filename ] command ...~%")
  (unless help
    (format t "~a: command line parse error.~%" (first args))
    (format t "~@[Error parsing args: ~{~s~^ ~}~%~]~%" (rest args)))
  (format t "~%Available commands:~%")
  (loop :for command :across *commands*
     :do (with-slots (verbs bindings help) command
           (format t " ~{~a~^ ~} ~{~a~^ ~}~28T~a~%" verbs bindings help))))


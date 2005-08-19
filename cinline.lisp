(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  ;; we use the elephant package to interface with BerkeleyDB,
  ;; in order to cache built C code
  ;; XXX is there a way to use ASDF for this instead?
  (asdf:operate 'asdf:load-op :elephant))

(defpackage :cinline
  (:nicknames :cin)
  (:documentation "Include inline C in your LISP code.
Stolen from Lush <http://lush.sf.net>")
  (:use :cl)
  #+cmu
  (:use :alien :c-call :system)
  #+sbcl
  (:use :sb-alien :sb-sys)
  (:export #:create-c-function
           #:cinline
           #:string->char*
           #:char*->string
           #:array->foreign
           :*CC* :*CC-args* :*libdir*
           :+deflibs+ :+defdir+))

(in-package :cinline)

;; add python-style multi-line strings
(eval-when (:execute :load-toplevel :compile-toplevel)

  (let ((normal-string-reader (get-macro-character #\")))
    (declare (type function normal-string-reader))
    (defun read-multiline-string (stream c)
      (let ((buffer ()))
        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            (funcall normal-string-reader stream c)))
        (read-char stream)

        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            ""))
        (read-char stream)

        (do ((chars (list (read-char stream)
                          (read-char stream)
                          (read-char stream))
                    (cdr (nconc chars (list (read-char stream))))))
            ((every #'(lambda (c) (eq c #\")) chars)
             (coerce (nreverse buffer) 'string))
          (push (car chars) buffer)))))

  (set-macro-character #\" #'read-multiline-string))

(eval-when (:compile-toplevel :load-toplevel :execute) ; goes for the whole file (almost)

(defparameter *CC* "gcc" "C compiler")
(defparameter *CC-args* '("-c" "-O3") "options to the C compiler")
(defparameter *libdir* "C" "location of .c and .o files")

(defparameter +defdir+  :in-out "default direction of arguments")
(defparameter +deflibs+ '("c")  "default set of libraries to load")

(defmacro dbind (mv l &body r)
  `(destructuring-bind ,mv ,l ,@r))

(defmacro mvbind (l mv &body r)
  `(multiple-value-bind ,l ,mv ,@r))

;;;

(defvar *typedefs* "
#define byte   char
#define ubyte  unsigned char
#define ushort unsigned short
#define uint   unsigned int
#define ulong  unsigned long")

;;;

(defmacro lisp-type (ctype)
  `(get ,ctype 'lisp-type))

(setf (lisp-type :char   ) 'character) ; XXX or should this be base_char?
(setf (lisp-type :char*  ) '(or null string))
(setf (lisp-type :byte   ) '(signed-byte 8))
(setf (lisp-type :byte*  ) '(or null (simple-array (signed-byte 8) (*))))
(setf (lisp-type :ubyte  ) '(unsigned-byte 8))
(setf (lisp-type :ubyte* ) '(or null (simple-array (unsigned-byte 8) (*))))
(setf (lisp-type :short  ) '(signed-byte 16))
(setf (lisp-type :short* ) '(or null (simple-array (signed-byte 16) (*))))
(setf (lisp-type :ushort ) '(unsigned-byte 16))
(setf (lisp-type :ushort*) '(or null (simple-array (unsigned-byte 16) (*))))
(setf (lisp-type :int    ) '(signed-byte 32))
(setf (lisp-type :int*   ) '(or null (simple-array (signed-byte 32) (*))))
(setf (lisp-type :uint   ) '(unsigned-byte 32))
(setf (lisp-type :uint*  ) '(or null (simple-array (unsigned-byte 32) (*))))
(setf (lisp-type :long   ) '(signed-byte 64))
(setf (lisp-type :long*  ) '(or null (simple-array (signed-byte 64) (*))))
(setf (lisp-type :ulong  ) '(unsigned-byte 64))
(setf (lisp-type :ulong* ) '(or null (simple-array (unsigned-byte 64) (*))))
(setf (lisp-type :float  ) 'single-float)
(setf (lisp-type :float* ) '(or null (simple-array single-float (*))))
(setf (lisp-type :double ) 'double-float)
(setf (lisp-type :double*) '(or null (simple-array double-float (*))))

;;;

(defmacro ffi-type (ctype)
  `(get ,ctype 'ffi-type))

(setf (ffi-type :char    ) '(signed 8))
(setf (ffi-type :char*   ) '(* (signed 8)))
(setf (ffi-type :byte    ) '(signed 8))
(setf (ffi-type :byte*   ) '(* (signed 8)))
(setf (ffi-type :ubyte   ) '(unsigned 8))
(setf (ffi-type :ubyte*  ) '(* (unsigned 8)))
(setf (ffi-type :short   ) '(signed 16))
(setf (ffi-type :short*  ) '(* (signed 16)))
(setf (ffi-type :ushort  ) '(unsigned 16))
(setf (ffi-type :ushort* ) '(* (unsigned 16)))
(setf (ffi-type :int     ) '(signed 32))
(setf (ffi-type :int*    ) '(* (signed 32)))
(setf (ffi-type :uint    ) '(unsigned 32))
(setf (ffi-type :uint*   ) '(* (unsigned 32)))
(setf (ffi-type :long    ) '(signed 64))
(setf (ffi-type :long*   ) '(* (signed 64)))
(setf (ffi-type :ulong   ) '(unsigned 64))
(setf (ffi-type :ulong*  ) '(* (unsigned 64)))
(setf (ffi-type :float   ) 'single-float)
(setf (ffi-type :float*  ) '(* single-float))
(setf (ffi-type :double  ) 'double-float)
(setf (ffi-type :double* ) '(* double-float))

;; we're also going to add a void* type, but--and this is SUCH a hack--we're
;; going to use this package to ask the compiler for the size.
;; XXX is there a more elegant way to get sizeof(void*)?

(defun strcat (&rest args)
  (apply #'concatenate (cons 'string args)))

(defun join (sep list)
   """ (join ", " '("abc" "def" "ghi")) -> "abc, def, ghi" """
   (declare (string sep)
            (list list))
   (if (null list)
       ""
     (reduce (lambda (acc str)
               (if (null str) acc (strcat acc sep str)))
             list)))

(defun pointerp (ctype)
  "Simply returns whether the input symbol ends with a * (except void*, which is opaque)"
  (if (eq ctype :void*)
      nil
    (let ((s (symbol-name ctype)))
      (char= #\* (char s (1- (length s)))))))

(defun symbol->keyword (sym)
  (read-from-string (strcat ":" (symbol-name sym))))
  
(defun lisp-sym->c-name (sym)
  (substitute #\_ #\-
              (string-downcase (symbol-name sym))))

(defun c-name->lisp-sym (cname)
  (read-from-string
   (substitute #\- #\_ cname)))

(defun is-direct-arg (arg)
  (dbind (ctype var dir) arg
    (declare (ignore var))
    (or (eq dir :in)
        (and (pointerp ctype)))))

(defun process-args (fn code)
  "Go through the code and whenever encountering a variable like $x,
   call (fn position-of-x x)"
  (declare (string code)
           (function fn))
  (let ((len (length code))
        (curvar (make-array 15 :element-type 'character
                            :fill-pointer 0 :adjustable t))
        ;; state = TOPLEVEL | INVAR | STRING | CHAR | MULTI-COMMENT | LINE-COMMENT
        (state :TOPLEVEL))
    (loop for i fixnum below len do
          (let ((c (char code i)))
            (ecase state
              (:TOPLEVEL (case c
                           (#\\ (incf i))
                           (#\" (setq state :STRING))
                           (#\' (setq state :CHAR))
                           (#\$ (setq state :INVAR)
                                (setf (fill-pointer curvar) 0))
                           (#\/ (when (< i (1- len))
                                  (case (char code (1+ i))
                                    (#\/ (setq state :LINE-COMMENT)
                                         (incf i))
                                    (#\* (setq state :MULTI-COMMENT)
                                         (incf i)))))))
              (:INVAR    (cond
                           ((or (char= #\_ c) (alphanumericp c))
                            (when (and (not (alpha-char-p c))
                                       (not (char= #\_ c))
                                       (= 0 (length curvar)))
                              (error "variable name cannot start with a number"))
                            (vector-push-extend c curvar)
                            (when (= i (1- len)) ; EOF
                              (funcall fn (- i (length curvar)) (copy-seq curvar))))
                           (t
                            (funcall fn (- i (1+ (length curvar))) (copy-seq curvar))
                            (setq state :TOPLEVEL)
                            (decf i))))
              (:STRING   (case c
                           (#\\ (incf i))
                           (#\" (setq state :TOPLEVEL))))
              (:CHAR     (case c
                           (#\\ (incf i))
                           (#\' (setq state :TOPLEVEL))))
              (:MULTI-COMMENT
               (when (= i (1- len))     ; EOF
                 (error "Non-terminating multi line comment"))
               (when (and (char= #\* c) (char= #\/ (char code (1+ i))))
                 (setq state :TOPLEVEL)
                 (incf i)))
              (:LINE-COMMENT
               (when (and (char= #\Newline c)
                          (char/= #\\ (char code (1- i))))
                 (setq state :TOPLEVEL)))))))
  code)

(defun arg-as-str (arg)
  (dbind (ctype var dir) arg
    (declare (ignore dir))
    (strcat (lisp-sym->c-name ctype)
            " " (unless (is-direct-arg arg) "*")
            (lisp-sym->c-name var))))

(defun create-code (cname args header body)
  (declare (type string cname header body))
  (let ((code (list (format nil "~A~&~%~A~%void ~A (~A) {~%" header *typedefs* cname
                            (join ", " (mapcar #'arg-as-str args)))))
        (codepos 0))
    (flet ((process-arg (pos varname)
             (let* ((var (c-name->lisp-sym varname))
                    (arg (find var args
                               :test (lambda (var arg)
                                       (eq var (cadr arg))))))
               (push (make-array (- pos codepos)
                                 :element-type 'character
                                 :displaced-to body
                                 :displaced-index-offset codepos)
                     code)
               (if (is-direct-arg arg)
                   (push varname code)
                 (push (strcat "(*" varname ")") code)))
             (setq codepos (+ 1 (length varname) pos))))
      (process-args  #'process-arg body))
    (push (make-array (- (length body) codepos)
                      :element-type 'character
                      :displaced-to body
                      :displaced-index-offset codepos)
          code)
    (push (format nil ";~&}~%") code)
    (apply #'strcat (reverse code))))

(defun create-lib (cname code)
  (let* ((source-fname (make-pathname :directory `(:relative ,*libdir*) :name cname :type "c"))
         (lib-fname    (make-pathname :directory `(:relative ,*libdir*) :name cname :type "o"))
         (CC-cmd (format nil "~A ~A~{ ~A~} -o ~A" *CC* source-fname *CC-args* lib-fname)))
    (with-open-file (strm source-fname :direction :output :if-exists :rename)
      (write-string code strm))
    (format t "Compiling...~%~A~%" CC-cmd)
    (unless (zerop (asdf:run-shell-command CC-cmd))
      (error "Compilation failed"))
    lib-fname))

(defun char*->string (cstr)
  ;; we're going to tag cstr with its the original length before adjusting
  (declare (type (or null (simple-array character (*))) cstr))
  (if (null cstr)
      nil
    (let ((end (position #\Null cstr)))
      (when (null end)
        (error "char* :out parameters must contain null bytes (use byte* instead)"))
      (adjust-array cstr end))))

(defun string->char* (str)
  (declare (type (or null (simple-array character (*))) str))
  (if (null str)
      nil
    (let* ((len (length str))
           (str (adjust-array str (1+ len))))
      (setf (char str len) #\Null)
      str)))

(defun array->foreign (array)
  (if (zerop (length array)) ; includes array = nil
    (int-sap 0)
    (vector-sap array)))
  
(defun create-wrapper (lname args)
  (let* ((in-args  (remove-if (lambda (arg)
                                (dbind (ctype var dir) arg
                                  (declare (ignore var))
                                  (and (not (pointerp ctype)) (eq dir :out))))
                              args))
         (in-strs  (remove-if (lambda (arg) 
                                (dbind (ctype var dir) arg
                                  (declare (ignore var))
                                  (or (not (eq ctype :char*)) (eq dir :out))))
                              in-args))
         (our-rets (remove-if (lambda (arg)
                                (dbind (ctype var dir) arg
                                  (declare (ignore var))
                                  (or (and (pointerp ctype) (not (eq ctype :char*)))
                                      (eq dir :in))))
                              args))
         (ffi-rets (mapcar #'second  ; names of vars the FFI function returns (no char*'s)
                           (remove-if (lambda (ctype) (eq ctype :char*))
                                      our-rets :key #'first)))
         (arrays   (mapcar #'second  ; names of array arguments
                           (remove-if-not #'pointerp args :key #'first)))
         (pointers (mapcar (lambda (arg) (declare (ignore arg)) (gensym)) arrays))
         (ffi-args (let ((ap arrays) (pp pointers)) ; names of in-args, with arrays replaced by pointers
                     (mapcar (lambda (arg)
                               (dbind (ctype var dir) arg
                                 (declare (ignore ctype dir))
                                 (if (eq var (car ap))
                                     (prog1
                                         (car pp)
                                       (setq ap (cdr ap) pp (cdr pp)))
                                   var)))
                             in-args)))
         (voidret  (gensym)))
    (eval
     `(lambda ,(mapcar #'second in-args)        ; the argument names
       (declare ,@(mapcar (lambda (arg)         ; declare types
                            (dbind (ctype var dir) arg
                              (declare (ignore dir))
                              `(type ,(lisp-type ctype) ,var)))
                          in-args))
       (let* (,@(mapcar (lambda (arg)           ; transform strings to char*'s
                          (dbind (ctype var dir) arg
                            (declare (ignore ctype dir))
                            `(,var (string->char* ,var))))
                        in-strs)
              ,@(mapcar (lambda (pointer array) ; transform lisp arrays to pointers
                          `(,pointer (array->foreign ,array)))
                        pointers arrays))
         (mvbind (,voidret ,@ffi-rets) (,lname ,@ffi-args)
            (declare (ignore ,voidret))
            (values ,@(mapcar (lambda (ret)     ; return values, transforming char*'s to strings
                                (dbind (ctype var dir) ret
                                  (declare (ignore dir))
                                  (if (eq ctype :char*)
                                      `(char*->string ,var)
                                    var)))
                              our-rets))))))))

(defvar *lib->fn* (make-hash-table :test #'equal) "Cache to avoid double-loading a library")

(defun create-key (args header body)
  (strcat (join "|" (mapcar (lambda (arg)
                              (dbind (ctype var dir) arg
                                (strcat (symbol-name ctype)
                                        "+" (symbol-name var)
                                        "+" (symbol-name dir))))
                            args))
          "--" header "--" body))

(defun load-lib (cname args libpath libs)
  (let ((libs-l (mapcar (lambda (lib) (strcat "-l" lib)) libs))
        (lname  (c-name->lisp-sym cname)))
    (format t "Loading ~A~%" libpath)
    (load-foreign libpath :libraries libs-l)
    ;; XXX is it evil that I'm using eval?
    (eval `(def-alien-routine (,cname ,lname) void
            ,@(mapcar (lambda (arg)
                        (dbind (ctype var dir) arg
                          (let ((dir (if (pointerp ctype) :in dir))
                                (ffi-type (ffi-type ctype)))
                            (when (null ffi-type)
                              (error "Type ~A not found." ctype))
                            (list var ffi-type dir))))
                      args)))
    (proclaim `(inline ,lname))
    lname))

(defun create-c-function (args header body &optional (libs +deflibs+))
  (ensure-directories-exist (strcat *libdir* "/"))
  (ele:with-open-store (*libdir*)
    (ele:with-transaction ()
      (let* ((args  (mapcar (lambda (arg) ; ensure that dir exists
                              (dbind (ctype var &optional (dir +defdir+)) arg
                                (list ctype var dir)))
                            args))
             (ckey  (create-key args header body))
             (cname (ele:get-from-root ckey)))
        (unless cname
          (let ((libcounter (ele:get-from-root "libcounter")))
            (declare (type (or null fixnum) libcounter))
            (unless libcounter (setq libcounter 0))
            (ele:add-to-root "libcounter" (1+ libcounter))
            (setq cname (strcat "c_func_" (write-to-string libcounter)))
            (let ((code (create-code cname args header body)))
              (create-lib cname code))
            (ele:add-to-root ckey cname)))
        (let* ((libkey (strcat cname (write-to-string libs)))
               (ret    (gethash libkey *lib->fn*)))
          (unless ret
            (setq ret (create-wrapper 
                       (load-lib cname args 
                                 (make-pathname :directory `(:relative ,*libdir*)
                                                :name cname :type "o")
                                 libs)
                       args))
            (setf (gethash libkey *lib->fn*) ret))
          ret)))))

(defmacro cinline (args header body &optional (libs +deflibs+))
  (let* ((args (mapcar (lambda (arg)
                         (cons (symbol->keyword (car arg)) (cdr arg)))
                       args))
         ;; the name must be unique for this session (gensym) and across sessions (time)
         (fn-wrapper (gensym (format nil "c-func-~A-" (get-universal-time))))
         (fn-args ())
         (fn-rets ()))
    (dolist (arg (reverse args))
      (dbind (ctype var &optional (dir +defdir+)) arg
        (when (or (pointerp ctype) (not (eq dir :out)))
          (push var fn-args))
        (when (and (not (eq dir :in))
                   (or (eq ctype :char*) (not (pointerp ctype))))
          (push (cons var (gensym)) fn-rets))))
    `(progn
      (unless (fboundp ',fn-wrapper)
        (setf (symbol-function ',fn-wrapper)
              (create-c-function ',args ,header ,body ',libs)))
      (mvbind ,(mapcar #'cdr fn-rets) (,fn-wrapper ,@fn-args)
        ,@(mapcar (lambda (x) `(setq ,(car x) ,(cdr x))) fn-rets))
      nil)))

(defun add-void* ()
  (let ((s 0))
    (cinline ((int s :out)) "" "$s = 8 * sizeof(void*)" nil)
    (setf (lisp-type :void*) `(unsigned-byte ,s))
    (setf (ffi-type  :void*) `(unsigned ,s))))

) ; end eval-when

;;; OK, now that we have access to the compiler, we can set up void*
(load-time-value
 (unless (lisp-type :void*)
   (add-void*))
 nil)
 

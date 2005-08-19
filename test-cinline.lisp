(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
  (asdf:operate 'asdf:load-op :cinline))

;;; Some standard C routines. Careful in slime, it redirects stdout

(defun system (cmd)
  (let ((ret 0))
    (cin:cinline ((int ret :out) (char* cmd :in))
                 "#include <unistd.h>"
                 "$ret = system ($cmd)")
    ret))

(defun malloc (size)
  (let ((p 0))
    (cin:cinline ((void* p :out) (int size :in))
                 "#include <stdlib.h>"
                 "$p = malloc ($size)")
    p))

(defun free (p)
  (cin:cinline ((void* p :in)) "#include <stdlib.h>" "free ($p)"))

(defun fopen (file mode)
  (let ((ret 0))
    (cin:cinline ((void* ret :out)
                  (char* file :in)
                  (char* mode :in))
                 "#include <stdio.h>"
                 "$ret = fopen ($file, $mode)")
    ret))

(defun popen (cmd mode)
  (let ((ret 0))
    (cin:cinline ((void* ret :out)
                  (char* cmd :in)
                  (char* mode :in))
                 "#include <stdio.h>"
                 "$ret = popen ($cmd, $mode)")
    ret))

(defun fread (ptr size nitems fp)
  (let ((ret 0))
    (cin:cinline ((byte* ptr)
                  (uint size :in)
                  (uint nitems :in)
                  (void* fp :in)
                  (int ret :out))
                 "#include <stdio.h>"
                 "$ret = fread ($ptr, $size, $nitems, $fp)")
    ret))

(defun fwrite (ptr size nitems fp)
  (let ((ret 0))
    (cin:cinline ((byte* ptr)
                  (uint size :in)
                  (uint nitems :in)
                  (void* fp :in)
                  (int ret :out))
                 "#include <stdio.h>"
                 "$ret = fwrite ($ptr, $size, $nitems, $fp)")
    ret))

(defun pclose (fp)
  (let ((ret 0))
    (cin:cinline ((void* fp :in) (int ret :out))
                 "#include <stdio.h>"
                 "$ret = pclose ($fp)")
    ret))

(defun fclose (fp)
  (let ((ret 0))
    (cin:cinline ((void* fp :in) (int ret :out))
                 "#include <stdio.h>"
                 "$ret = fclose ($fp)")
    ret))

(defun perror (s)
  (cin:cinline ((char* s :in)) "#include <stdio.h>" "perror ($s)"))
               
      
;;; Use fwrite and a lower level function (array->foreign)

(defun formatf (destfp control-string &rest format-args)
  (let ((s (apply 'format `(nil ,control-string ,@format-args))))
    (fwrite (cin:array->foreign s) 1 (length s) destfp)))

;;; A more involved example

(defun symm-eigenvals (matrix)
  "Use GSL and BLAS to find the eigenvalues of a symmetric matrix

Usage: (symm-eigenvals 
         (make-array 4 :initial-contents '(1d0 2d0 2d0 1d0)
                       :element-type 'double-float))
==>    #(3.0000000000000004d0 -1.0000000000000002d0) 
"
  (declare (type (simple-array double-float (*)) matrix))
  (let* ((n^2 (length matrix))
         (n-r (multiple-value-list (floor (sqrt n^2))))
         (n   (if (zerop (cadr n-r)) (car n-r) (error "matrix isn't square")))
         (eivals (make-array n :element-type 'double-float)))
    (cin:cinline ((double* matrix) (uint n) (double* eivals))
                 """
#include <gsl/gsl_math.h>
#include <gsl/gsl_eigen.h>
                 """
                 """
gsl_matrix_view m = gsl_matrix_view_array ($matrix, $n, $n);
gsl_vector_view o = gsl_vector_view_array ($eivals, $n);
gsl_eigen_symm_workspace *w = gsl_eigen_symm_alloc ($n);

gsl_eigen_symm (&m.matrix, &o.vector, w);
gsl_eigen_symm_free (w);
                 """
                 ("gsl" "blas"))
    eivals))

;;; Run a command, and process the output a line at a time

(defun process-by-line (fn cmd &optional (buf-size 1024))
  (let ((fp (popen cmd "r"))
        (line (make-string buf-size))
        (read-err 0))
    (if (zerop fp)
        nil
;      (unwind-protect
      (progn
           (loop while (zerop read-err) do
                 ;; line is a byte* and not a char* because we're using it
                 ;; as a buffer, so we don't need char* :in behavior and we
                 ;; definitely don't want char* :out behavior: shrinking line
                 ;; on the lisp side to strlen (line).
                 (cin:cinline ((void* fp :in)
                               (byte* line)
                               (int buf-size :in)
                               (int read-err :out))
                              "#include <stdio.h>"
                              """
                              if (fgets ($line, $buf_size, $fp))
                              {
                                int len = strlen ($line);
                                if (len > 0 && $line[len-1] == '\n')
                                  $line[len-1] = '\0';
                              }
                              else
                                $read_err = feof ($fp) ? 1 : -1;
                              """)
                 (when (zerop read-err)
                   (funcall fn (cin:char*->string line))))
        (pclose fp)))
    (> read-err 0)))
           

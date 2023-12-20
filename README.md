# Inline C for Common Lisp

## Introduction
Inline C is an idea taken from [Lush](http://lush.sf.net) - namely, the ability to just switch over to C when anything is slow or awkward in plain LISP. This is particularly useful when using outside libraries, because no LISP bindings are necessary.

## Example
Here's an example of the usage (taken from `test-cinline.lisp`):
```lisp
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
```

## Status
Right now, Inline C is only implemented for CMUCL and SBCL. UFFI was my first choice, but it doesn't give you access to the data pointer in a vector.

## Future Direction
Look at CFFI for portability
Provide the ability to place different pieces of C code in one compilation unit

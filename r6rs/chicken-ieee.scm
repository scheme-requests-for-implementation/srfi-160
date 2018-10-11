;;;; IEEE float<->bytevector access for Chicken
;;; Author: Felix Winkelmann

(define bytevector-ieee-single-native-ref
  (foreign-lambda* float ((u8vector bv) (int offset))
    "float f;"
    "C_memcpy(&f, bv + offset, sizeof(float));"
    "C_return(f);"))

(define bytevector-ieee-double-native-ref
  (foreign-lambda* double ((u8vector bv) (int offset))
    "double f;"
    "C_memcpy(&f, bv + offset, sizeof(double));"
    "C_return(f);"))

(define bytevector-ieee-single-native-set!
  (foreign-lambda* void ((u8vector bv) (int offset) (float x))
    "C_memcpy(bv + offset, &x, sizeof(float));"))

(define bytevector-ieee-double-native-set!
  (foreign-lambda* void ((u8vector bv) (int offset) (double x))
    "C_memcpy(bv + offset, &x, sizeof(double));"))


((define _eqv?_15 eqv?)
 (define _cons_30 cons)
 (define _append_31 append)
 (define _list_32 list)
 (define _vector_33 vector)
 (define _list->vector_34 list->vector)
 (define _map_35 map)
 (define ? any/c)
 (define pt/c (vector/c flonum? flonum? flonum?))
 (define tfo/c
   (vector/c
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?
    flonum?))
 (define nuc/c
   (vector/c
    tfo/c
    tfo/c
    tfo/c
    tfo/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c))
 (define rA/c
   (vector/c
    (one-of/c 'rA)
    tfo/c
    tfo/c
    tfo/c
    tfo/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c))
 (define rC/c
   (vector/c
    (one-of/c 'rC)
    tfo/c
    tfo/c
    tfo/c
    tfo/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c))
 (define rG/c
   (vector/c
    (one-of/c 'rG)
    tfo/c
    tfo/c
    tfo/c
    tfo/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c))
 (define rU/c
   (vector/c
    (one-of/c 'rU)
    tfo/c
    tfo/c
    tfo/c
    tfo/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c
    pt/c))
 (define var/c (vector/c integer? tfo/c nuc/c))
 (define constant-pi 3.141592653589793)
 (define constant-minus-pi -3.141592653589793)
 (define constant-pi/2 1.5707963267948966)
 (define constant-minus-pi/2 -1.5707963267948966)
 (define math-atan2
   (lambda (_y_48 _x_49)
     (if (> _x_49 0.0)
       (atan (/ _y_48 _x_49))
       (if (< _y_48 0.0)
         (if (= _x_49 0.0)
           constant-minus-pi/2
           (+ (atan (/ _y_48 _x_49)) constant-minus-pi))
         (if (= _x_49 0.0)
           constant-pi/2
           (+ (atan (/ _y_48 _x_49)) constant-pi))))))
 (define pt-sub
   (lambda (_p1_48 _p2_49)
     (vector
      (- (vector-ref _p1_48 0) (vector-ref _p2_49 0))
      (- (vector-ref _p1_48 1) (vector-ref _p2_49 1))
      (- (vector-ref _p1_48 2) (vector-ref _p2_49 2)))))
 (define pt-dist
   (lambda (_p1_48 _p2_49)
     ((lambda (_dx_50 _dy_51 _dz_52)
        (sqrt (+ (* _dx_50 _dx_50) (* _dy_51 _dy_51) (* _dz_52 _dz_52))))
      (- (vector-ref _p1_48 0) (vector-ref _p2_49 0))
      (- (vector-ref _p1_48 1) (vector-ref _p2_49 1))
      (- (vector-ref _p1_48 2) (vector-ref _p2_49 2)))))
 (define pt-phi
   (lambda (_p_48)
     ((lambda (_x_49)
        ((lambda (_y_50)
           ((lambda (_z_51)
              ((lambda (_b_52)
                 (math-atan2
                  (+ (* (cos _b_52) _z_51) (* (sin _b_52) _x_49))
                  _y_50))
               (math-atan2 _x_49 _z_51)))
            (vector-ref _p_48 2)))
         (vector-ref _p_48 1)))
      (vector-ref _p_48 0))))
 (define pt-theta
   (lambda (_p_48) (math-atan2 (vector-ref _p_48 0) (vector-ref _p_48 2))))
 (define tfo-id '#(1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0))
 (define tfo-apply
   (lambda (_tfo_48 _p_49)
     ((lambda (_x_50 _y_51 _z_52)
        (vector
         (+
          (* _x_50 (vector-ref _tfo_48 0))
          (* _y_51 (vector-ref _tfo_48 3))
          (* _z_52 (vector-ref _tfo_48 6))
          (vector-ref _tfo_48 9))
         (+
          (* _x_50 (vector-ref _tfo_48 1))
          (* _y_51 (vector-ref _tfo_48 4))
          (* _z_52 (vector-ref _tfo_48 7))
          (vector-ref _tfo_48 10))
         (+
          (* _x_50 (vector-ref _tfo_48 2))
          (* _y_51 (vector-ref _tfo_48 5))
          (* _z_52 (vector-ref _tfo_48 8))
          (vector-ref _tfo_48 11))))
      (vector-ref _p_49 0)
      (vector-ref _p_49 1)
      (vector-ref _p_49 2))))
 (define tfo-combine
   (lambda (_A_48 _B_49)
     (vector
      (+
       (* (vector-ref _A_48 0) (vector-ref _B_49 0))
       (* (vector-ref _A_48 1) (vector-ref _B_49 3))
       (* (vector-ref _A_48 2) (vector-ref _B_49 6)))
      (+
       (* (vector-ref _A_48 0) (vector-ref _B_49 1))
       (* (vector-ref _A_48 1) (vector-ref _B_49 4))
       (* (vector-ref _A_48 2) (vector-ref _B_49 7)))
      (+
       (* (vector-ref _A_48 0) (vector-ref _B_49 2))
       (* (vector-ref _A_48 1) (vector-ref _B_49 5))
       (* (vector-ref _A_48 2) (vector-ref _B_49 8)))
      (+
       (* (vector-ref _A_48 3) (vector-ref _B_49 0))
       (* (vector-ref _A_48 4) (vector-ref _B_49 3))
       (* (vector-ref _A_48 5) (vector-ref _B_49 6)))
      (+
       (* (vector-ref _A_48 3) (vector-ref _B_49 1))
       (* (vector-ref _A_48 4) (vector-ref _B_49 4))
       (* (vector-ref _A_48 5) (vector-ref _B_49 7)))
      (+
       (* (vector-ref _A_48 3) (vector-ref _B_49 2))
       (* (vector-ref _A_48 4) (vector-ref _B_49 5))
       (* (vector-ref _A_48 5) (vector-ref _B_49 8)))
      (+
       (* (vector-ref _A_48 6) (vector-ref _B_49 0))
       (* (vector-ref _A_48 7) (vector-ref _B_49 3))
       (* (vector-ref _A_48 8) (vector-ref _B_49 6)))
      (+
       (* (vector-ref _A_48 6) (vector-ref _B_49 1))
       (* (vector-ref _A_48 7) (vector-ref _B_49 4))
       (* (vector-ref _A_48 8) (vector-ref _B_49 7)))
      (+
       (* (vector-ref _A_48 6) (vector-ref _B_49 2))
       (* (vector-ref _A_48 7) (vector-ref _B_49 5))
       (* (vector-ref _A_48 8) (vector-ref _B_49 8)))
      (+
       (* (vector-ref _A_48 9) (vector-ref _B_49 0))
       (* (vector-ref _A_48 10) (vector-ref _B_49 3))
       (* (vector-ref _A_48 11) (vector-ref _B_49 6))
       (vector-ref _B_49 9))
      (+
       (* (vector-ref _A_48 9) (vector-ref _B_49 1))
       (* (vector-ref _A_48 10) (vector-ref _B_49 4))
       (* (vector-ref _A_48 11) (vector-ref _B_49 7))
       (vector-ref _B_49 10))
      (+
       (* (vector-ref _A_48 9) (vector-ref _B_49 2))
       (* (vector-ref _A_48 10) (vector-ref _B_49 5))
       (* (vector-ref _A_48 11) (vector-ref _B_49 8))
       (vector-ref _B_49 11)))))
 (define tfo-inv-ortho
   (lambda (_tfo_48)
     ((lambda (_tx_49)
        ((lambda (_ty_50)
           ((lambda (_tz_51)
              (vector
               (vector-ref _tfo_48 0)
               (vector-ref _tfo_48 3)
               (vector-ref _tfo_48 6)
               (vector-ref _tfo_48 1)
               (vector-ref _tfo_48 4)
               (vector-ref _tfo_48 7)
               (vector-ref _tfo_48 2)
               (vector-ref _tfo_48 5)
               (vector-ref _tfo_48 8)
               (-
                (+
                 (* (vector-ref _tfo_48 0) _tx_49)
                 (* (vector-ref _tfo_48 1) _ty_50)
                 (* (vector-ref _tfo_48 2) _tz_51)))
               (-
                (+
                 (* (vector-ref _tfo_48 3) _tx_49)
                 (* (vector-ref _tfo_48 4) _ty_50)
                 (* (vector-ref _tfo_48 5) _tz_51)))
               (-
                (+
                 (* (vector-ref _tfo_48 6) _tx_49)
                 (* (vector-ref _tfo_48 7) _ty_50)
                 (* (vector-ref _tfo_48 8) _tz_51)))))
            (vector-ref _tfo_48 11)))
         (vector-ref _tfo_48 10)))
      (vector-ref _tfo_48 9))))
 (define tfo-align
   (lambda (_p1_48 _p2_49 _p3_50)
     ((lambda (_x1_51)
        ((lambda (_y1_52)
           ((lambda (_z1_53)
              ((lambda (_x3_54)
                 ((lambda (_y3_55)
                    ((lambda (_z3_56)
                       ((lambda (_x31_57)
                          ((lambda (_y31_58)
                             ((lambda (_z31_59)
                                ((lambda (_rotpY_60)
                                   ((lambda (_Phi_61)
                                      ((lambda (_Theta_62)
                                         ((lambda (_sinP_63)
                                            ((lambda (_sinT_64)
                                               ((lambda (_cosP_65)
                                                  ((lambda (_cosT_66)
                                                     ((lambda (_sinPsinT_67)
                                                        ((lambda (_sinPcosT_68)
                                                           ((lambda (_cosPsinT_69)
                                                              ((lambda (_cosPcosT_70)
                                                                 ((lambda (_rotpZ_71)
                                                                    ((lambda (_Rho_72)
                                                                       ((lambda (_cosR_73)
                                                                          ((lambda (_sinR_74)
                                                                             ((lambda (_x_75)
                                                                                ((lambda (_y_76)
                                                                                   ((lambda (_z_77)
                                                                                      (vector
                                                                                       (-
                                                                                        (*
                                                                                         _cosT_66
                                                                                         _cosR_73)
                                                                                        (*
                                                                                         _cosPsinT_69
                                                                                         _sinR_74))
                                                                                       _sinPsinT_67
                                                                                       (+
                                                                                        (*
                                                                                         _cosT_66
                                                                                         _sinR_74)
                                                                                        (*
                                                                                         _cosPsinT_69
                                                                                         _cosR_73))
                                                                                       (*
                                                                                        _sinP_63
                                                                                        _sinR_74)
                                                                                       _cosP_65
                                                                                       (-
                                                                                        (*
                                                                                         _sinP_63
                                                                                         _cosR_73))
                                                                                       (-
                                                                                        (-
                                                                                         (*
                                                                                          _sinT_64
                                                                                          _cosR_73))
                                                                                        (*
                                                                                         _cosPcosT_70
                                                                                         _sinR_74))
                                                                                       _sinPcosT_68
                                                                                       (+
                                                                                        (-
                                                                                         (*
                                                                                          _sinT_64
                                                                                          _sinR_74))
                                                                                        (*
                                                                                         _cosPcosT_70
                                                                                         _cosR_73))
                                                                                       (-
                                                                                        (*
                                                                                         _x_75
                                                                                         _cosR_73)
                                                                                        (*
                                                                                         _z_77
                                                                                         _sinR_74))
                                                                                       _y_76
                                                                                       (+
                                                                                        (*
                                                                                         _x_75
                                                                                         _sinR_74)
                                                                                        (*
                                                                                         _z_77
                                                                                         _cosR_73))))
                                                                                    (-
                                                                                     (+
                                                                                      (-
                                                                                       (*
                                                                                        _x1_51
                                                                                        _cosPsinT_69))
                                                                                      (*
                                                                                       _y1_52
                                                                                       _sinP_63))
                                                                                     (*
                                                                                      _z1_53
                                                                                      _cosPcosT_70))))
                                                                                 (-
                                                                                  (-
                                                                                   (-
                                                                                    (*
                                                                                     _x1_51
                                                                                     _sinPsinT_67))
                                                                                   (*
                                                                                    _y1_52
                                                                                    _cosP_65))
                                                                                  (*
                                                                                   _z1_53
                                                                                   _sinPcosT_68))))
                                                                              (+
                                                                               (-
                                                                                (*
                                                                                 _x1_51
                                                                                 _cosT_66))
                                                                               (*
                                                                                _z1_53
                                                                                _sinT_64))))
                                                                           (sin
                                                                            _Rho_72)))
                                                                        (cos
                                                                         _Rho_72)))
                                                                     (pt-theta
                                                                      _rotpZ_71)))
                                                                  (vector
                                                                   (-
                                                                    (*
                                                                     _cosT_66
                                                                     _x31_57)
                                                                    (*
                                                                     _sinT_64
                                                                     _z31_59))
                                                                   (+
                                                                    (*
                                                                     _sinPsinT_67
                                                                     _x31_57)
                                                                    (*
                                                                     _cosP_65
                                                                     _y31_58)
                                                                    (*
                                                                     _sinPcosT_68
                                                                     _z31_59))
                                                                   (+
                                                                    (*
                                                                     _cosPsinT_69
                                                                     _x31_57)
                                                                    (-
                                                                     (*
                                                                      _sinP_63
                                                                      _y31_58))
                                                                    (*
                                                                     _cosPcosT_70
                                                                     _z31_59)))))
                                                               (*
                                                                _cosP_65
                                                                _cosT_66)))
                                                            (*
                                                             _cosP_65
                                                             _sinT_64)))
                                                         (*
                                                          _sinP_63
                                                          _cosT_66)))
                                                      (* _sinP_63 _sinT_64)))
                                                   (cos _Theta_62)))
                                                (cos _Phi_61)))
                                             (sin _Theta_62)))
                                          (sin _Phi_61)))
                                       (pt-theta _rotpY_60)))
                                    (pt-phi _rotpY_60)))
                                 (pt-sub _p2_49 _p1_48)))
                              (- _z3_56 _z1_53)))
                           (- _y3_55 _y1_52)))
                        (- _x3_54 _x1_51)))
                     (vector-ref _p3_50 2)))
                  (vector-ref _p3_50 1)))
               (vector-ref _p3_50 0)))
            (vector-ref _p1_48 2)))
         (vector-ref _p1_48 1)))
      (vector-ref _p1_48 0))))
 (define get-var
   (lambda (_id_48 _lst_49)
     ((lambda (_v_50)
        (if (= _id_48 (vector-ref _v_50 0))
          _v_50
          (get-var _id_48 (cdr _lst_49))))
      (car _lst_49))))
 (define make-relative-nuc
   (lambda (_tfo_48 _n_49)
     (if (if (vector? _n_49) (eq? (vector-ref _n_49 0) 'rA) #f)
       (vector
        'rA
        (vector-ref _n_49 1)
        (vector-ref _n_49 2)
        (vector-ref _n_49 3)
        (vector-ref _n_49 4)
        (tfo-apply _tfo_48 (vector-ref _n_49 5))
        (tfo-apply _tfo_48 (vector-ref _n_49 6))
        (tfo-apply _tfo_48 (vector-ref _n_49 7))
        (tfo-apply _tfo_48 (vector-ref _n_49 8))
        (tfo-apply _tfo_48 (vector-ref _n_49 9))
        (tfo-apply _tfo_48 (vector-ref _n_49 10))
        (tfo-apply _tfo_48 (vector-ref _n_49 11))
        (tfo-apply _tfo_48 (vector-ref _n_49 12))
        (tfo-apply _tfo_48 (vector-ref _n_49 13))
        (tfo-apply _tfo_48 (vector-ref _n_49 14))
        (tfo-apply _tfo_48 (vector-ref _n_49 15))
        (tfo-apply _tfo_48 (vector-ref _n_49 16))
        (tfo-apply _tfo_48 (vector-ref _n_49 17))
        (tfo-apply _tfo_48 (vector-ref _n_49 18))
        (tfo-apply _tfo_48 (vector-ref _n_49 19))
        (tfo-apply _tfo_48 (vector-ref _n_49 20))
        (tfo-apply _tfo_48 (vector-ref _n_49 21))
        (tfo-apply _tfo_48 (vector-ref _n_49 22))
        (tfo-apply _tfo_48 (vector-ref _n_49 23))
        (tfo-apply _tfo_48 (vector-ref _n_49 24))
        (tfo-apply _tfo_48 (vector-ref _n_49 25))
        (tfo-apply _tfo_48 (vector-ref _n_49 26))
        (tfo-apply _tfo_48 (vector-ref _n_49 27))
        (tfo-apply _tfo_48 (vector-ref _n_49 28))
        (tfo-apply _tfo_48 (vector-ref _n_49 29))
        (tfo-apply _tfo_48 (vector-ref _n_49 30))
        (tfo-apply _tfo_48 (vector-ref _n_49 31))
        (tfo-apply _tfo_48 (vector-ref _n_49 32))
        (tfo-apply _tfo_48 (vector-ref _n_49 33))
        (tfo-apply _tfo_48 (vector-ref _n_49 34))
        (tfo-apply _tfo_48 (vector-ref _n_49 35))
        (tfo-apply _tfo_48 (vector-ref _n_49 36))
        (tfo-apply _tfo_48 (vector-ref _n_49 37)))
       (if (if (vector? _n_49) (eq? (vector-ref _n_49 0) 'rC) #f)
         (vector
          'rC
          (vector-ref _n_49 1)
          (vector-ref _n_49 2)
          (vector-ref _n_49 3)
          (vector-ref _n_49 4)
          (tfo-apply _tfo_48 (vector-ref _n_49 5))
          (tfo-apply _tfo_48 (vector-ref _n_49 6))
          (tfo-apply _tfo_48 (vector-ref _n_49 7))
          (tfo-apply _tfo_48 (vector-ref _n_49 8))
          (tfo-apply _tfo_48 (vector-ref _n_49 9))
          (tfo-apply _tfo_48 (vector-ref _n_49 10))
          (tfo-apply _tfo_48 (vector-ref _n_49 11))
          (tfo-apply _tfo_48 (vector-ref _n_49 12))
          (tfo-apply _tfo_48 (vector-ref _n_49 13))
          (tfo-apply _tfo_48 (vector-ref _n_49 14))
          (tfo-apply _tfo_48 (vector-ref _n_49 15))
          (tfo-apply _tfo_48 (vector-ref _n_49 16))
          (tfo-apply _tfo_48 (vector-ref _n_49 17))
          (tfo-apply _tfo_48 (vector-ref _n_49 18))
          (tfo-apply _tfo_48 (vector-ref _n_49 19))
          (tfo-apply _tfo_48 (vector-ref _n_49 20))
          (tfo-apply _tfo_48 (vector-ref _n_49 21))
          (tfo-apply _tfo_48 (vector-ref _n_49 22))
          (tfo-apply _tfo_48 (vector-ref _n_49 23))
          (tfo-apply _tfo_48 (vector-ref _n_49 24))
          (tfo-apply _tfo_48 (vector-ref _n_49 25))
          (tfo-apply _tfo_48 (vector-ref _n_49 26))
          (tfo-apply _tfo_48 (vector-ref _n_49 27))
          (tfo-apply _tfo_48 (vector-ref _n_49 28))
          (tfo-apply _tfo_48 (vector-ref _n_49 29))
          (tfo-apply _tfo_48 (vector-ref _n_49 30))
          (tfo-apply _tfo_48 (vector-ref _n_49 31))
          (tfo-apply _tfo_48 (vector-ref _n_49 32))
          (tfo-apply _tfo_48 (vector-ref _n_49 33))
          (tfo-apply _tfo_48 (vector-ref _n_49 34))
          (tfo-apply _tfo_48 (vector-ref _n_49 35)))
         (if (if (vector? _n_49) (eq? (vector-ref _n_49 0) 'rG) #f)
           (vector
            'rG
            (vector-ref _n_49 1)
            (vector-ref _n_49 2)
            (vector-ref _n_49 3)
            (vector-ref _n_49 4)
            (tfo-apply _tfo_48 (vector-ref _n_49 5))
            (tfo-apply _tfo_48 (vector-ref _n_49 6))
            (tfo-apply _tfo_48 (vector-ref _n_49 7))
            (tfo-apply _tfo_48 (vector-ref _n_49 8))
            (tfo-apply _tfo_48 (vector-ref _n_49 9))
            (tfo-apply _tfo_48 (vector-ref _n_49 10))
            (tfo-apply _tfo_48 (vector-ref _n_49 11))
            (tfo-apply _tfo_48 (vector-ref _n_49 12))
            (tfo-apply _tfo_48 (vector-ref _n_49 13))
            (tfo-apply _tfo_48 (vector-ref _n_49 14))
            (tfo-apply _tfo_48 (vector-ref _n_49 15))
            (tfo-apply _tfo_48 (vector-ref _n_49 16))
            (tfo-apply _tfo_48 (vector-ref _n_49 17))
            (tfo-apply _tfo_48 (vector-ref _n_49 18))
            (tfo-apply _tfo_48 (vector-ref _n_49 19))
            (tfo-apply _tfo_48 (vector-ref _n_49 20))
            (tfo-apply _tfo_48 (vector-ref _n_49 21))
            (tfo-apply _tfo_48 (vector-ref _n_49 22))
            (tfo-apply _tfo_48 (vector-ref _n_49 23))
            (tfo-apply _tfo_48 (vector-ref _n_49 24))
            (tfo-apply _tfo_48 (vector-ref _n_49 25))
            (tfo-apply _tfo_48 (vector-ref _n_49 26))
            (tfo-apply _tfo_48 (vector-ref _n_49 27))
            (tfo-apply _tfo_48 (vector-ref _n_49 28))
            (tfo-apply _tfo_48 (vector-ref _n_49 29))
            (tfo-apply _tfo_48 (vector-ref _n_49 30))
            (tfo-apply _tfo_48 (vector-ref _n_49 31))
            (tfo-apply _tfo_48 (vector-ref _n_49 32))
            (tfo-apply _tfo_48 (vector-ref _n_49 33))
            (tfo-apply _tfo_48 (vector-ref _n_49 34))
            (tfo-apply _tfo_48 (vector-ref _n_49 35))
            (tfo-apply _tfo_48 (vector-ref _n_49 36))
            (tfo-apply _tfo_48 (vector-ref _n_49 37))
            (tfo-apply _tfo_48 (vector-ref _n_49 38)))
           (vector
            'rU
            (vector-ref _n_49 1)
            (vector-ref _n_49 2)
            (vector-ref _n_49 3)
            (vector-ref _n_49 4)
            (tfo-apply _tfo_48 (vector-ref _n_49 5))
            (tfo-apply _tfo_48 (vector-ref _n_49 6))
            (tfo-apply _tfo_48 (vector-ref _n_49 7))
            (tfo-apply _tfo_48 (vector-ref _n_49 8))
            (tfo-apply _tfo_48 (vector-ref _n_49 9))
            (tfo-apply _tfo_48 (vector-ref _n_49 10))
            (tfo-apply _tfo_48 (vector-ref _n_49 11))
            (tfo-apply _tfo_48 (vector-ref _n_49 12))
            (tfo-apply _tfo_48 (vector-ref _n_49 13))
            (tfo-apply _tfo_48 (vector-ref _n_49 14))
            (tfo-apply _tfo_48 (vector-ref _n_49 15))
            (tfo-apply _tfo_48 (vector-ref _n_49 16))
            (tfo-apply _tfo_48 (vector-ref _n_49 17))
            (tfo-apply _tfo_48 (vector-ref _n_49 18))
            (tfo-apply _tfo_48 (vector-ref _n_49 19))
            (tfo-apply _tfo_48 (vector-ref _n_49 20))
            (tfo-apply _tfo_48 (vector-ref _n_49 21))
            (tfo-apply _tfo_48 (vector-ref _n_49 22))
            (tfo-apply _tfo_48 (vector-ref _n_49 23))
            (tfo-apply _tfo_48 (vector-ref _n_49 24))
            (tfo-apply _tfo_48 (vector-ref _n_49 25))
            (tfo-apply _tfo_48 (vector-ref _n_49 26))
            (tfo-apply _tfo_48 (vector-ref _n_49 27))
            (tfo-apply _tfo_48 (vector-ref _n_49 28))
            (tfo-apply _tfo_48 (vector-ref _n_49 29))
            (tfo-apply _tfo_48 (vector-ref _n_49 30))
            (tfo-apply _tfo_48 (vector-ref _n_49 31))
            (tfo-apply _tfo_48 (vector-ref _n_49 32))
            (tfo-apply _tfo_48 (vector-ref _n_49 33))
            (tfo-apply _tfo_48 (vector-ref _n_49 34))))))))
 (define search
   (lambda (_partial-inst_48 _domains_49 _constraint?_50)
     (if (null? _domains_49)
       (list _partial-inst_48)
       ((lambda (_remaining-domains_51)
          (letrec ((_try-assignments_52
                    (lambda (_lst_53)
                      (if (null? _lst_53)
                        '()
                        ((lambda (_var_54)
                           (if (_constraint?_50 _var_54 _partial-inst_48)
                             ((lambda (_subsols1_55)
                                ((lambda (_subsols2_56)
                                   (append _subsols1_55 _subsols2_56))
                                 (_try-assignments_52 (cdr _lst_53))))
                              (search
                               (cons _var_54 _partial-inst_48)
                               _remaining-domains_51
                               _constraint?_50))
                             (_try-assignments_52 (cdr _lst_53))))
                         (car _lst_53))))))
            (_try-assignments_52 ((car _domains_49) _partial-inst_48))))
        (cdr _domains_49)))))
 (define dgf-base
   (lambda (_tfo_48 _ref_49 _nuc_50)
     ((lambda (_ref-nuc_51)
        ((lambda (_align_52)
           (tfo-combine
            (vector-ref _nuc_50 1)
            (tfo-combine _tfo_48 _align_52)))
         (tfo-inv-ortho
          (if (if (vector? _ref-nuc_51)
                (eq? (vector-ref _ref-nuc_51 0) 'rA)
                #f)
            (tfo-align
             ((lambda (_v_52)
                (tfo-apply
                 (vector-ref _v_52 1)
                 (vector-ref (vector-ref _v_52 2) 15)))
              _ref_49)
             ((lambda (_v_52)
                (tfo-apply
                 (vector-ref _v_52 1)
                 (vector-ref (vector-ref _v_52 2) 32)))
              _ref_49)
             ((lambda (_v_52)
                (tfo-apply
                 (vector-ref _v_52 1)
                 (vector-ref (vector-ref _v_52 2) 27)))
              _ref_49))
            (if (if (vector? _ref-nuc_51)
                  (eq? (vector-ref _ref-nuc_51 0) 'rC)
                  #f)
              (tfo-align
               ((lambda (_v_52)
                  (tfo-apply
                   (vector-ref _v_52 1)
                   (vector-ref (vector-ref _v_52 2) 15)))
                _ref_49)
               ((lambda (_v_52)
                  (tfo-apply
                   (vector-ref _v_52 1)
                   (vector-ref (vector-ref _v_52 2) 24)))
                _ref_49)
               ((lambda (_v_52)
                  (tfo-apply
                   (vector-ref _v_52 1)
                   (vector-ref (vector-ref _v_52 2) 26)))
                _ref_49))
              (if (if (vector? _ref-nuc_51)
                    (eq? (vector-ref _ref-nuc_51 0) 'rG)
                    #f)
                (tfo-align
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 15)))
                  _ref_49)
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 32)))
                  _ref_49)
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 27)))
                  _ref_49))
                (tfo-align
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 15)))
                  _ref_49)
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 24)))
                  _ref_49)
                 ((lambda (_v_52)
                    (tfo-apply
                     (vector-ref _v_52 1)
                     (vector-ref (vector-ref _v_52 2) 26)))
                  _ref_49))))))))
      (vector-ref _ref_49 2))))
 (define reference
   (lambda (_nuc_48 _i_49)
     (lambda (_partial-inst_50) (list (vector _i_49 tfo-id _nuc_48)))))
 (define wc-tfo
   '#(-1.0
      0.0028
      -0.0019
      0.0028
      0.3468
      -0.9379
      -0.0019
      -0.9379
      -0.3468
      -0.008
      6.073
      8.7208))
 (define wc
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (list (vector _i_49 _tfo_53 _nuc_48)))
           (dgf-base wc-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define wc-Dumas-tfo
   '#(-0.9737
      -0.1834
      0.1352
      -0.1779
      0.2417
      -0.9539
      0.1422
      -0.9529
      -0.2679
      0.4837
      6.2649
      8.0285))
 (define wc-Dumas
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (list (vector _i_49 _tfo_53 _nuc_48)))
           (dgf-base wc-Dumas-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define helix5*-tfo
   '#(0.9886
      -0.0961
      0.1156
      0.1424
      0.8452
      -0.5152
      -0.0482
      0.5258
      0.8492
      -3.8737
      0.548
      3.8024))
 (define helix5*
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (list (vector _i_49 _tfo_53 _nuc_48)))
           (dgf-base helix5*-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define helix3*-tfo
   '#(0.9886
      0.1424
      -0.0482
      -0.0961
      0.8452
      0.5258
      0.1156
      -0.5152
      0.8492
      3.4426
      2.0474
      -3.7042))
 (define helix3*
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (list (vector _i_49 _tfo_53 _nuc_48)))
           (dgf-base helix3*-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define G37-A38-tfo
   '#(0.9991
      0.0164
      -0.0387
      -0.0375
      0.7616
      -0.647
      0.0189
      0.6478
      0.7615
      -3.3018
      0.9975
      2.5585))
 (define G37-A38
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (vector _i_49 _tfo_53 _nuc_48))
           (dgf-base G37-A38-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define stacked5*
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       (cons
        ((G37-A38 _nuc_48 _i_49 _j_50) _partial-inst_51)
        ((helix5* _nuc_48 _i_49 _j_50) _partial-inst_51)))))
 (define A38-G37-tfo
   '#(0.9991
      -0.0375
      0.0189
      0.0164
      0.7616
      0.6478
      -0.0387
      -0.647
      0.7615
      3.3819
      0.7718
      -2.5321))
 (define A38-G37
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_tfo_53) (vector _i_49 _tfo_53 _nuc_48))
           (dgf-base A38-G37-tfo _ref_52 _nuc_48)))
        (get-var _j_50 _partial-inst_51)))))
 (define stacked3*
   (lambda (_nuc_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       (cons
        ((A38-G37 _nuc_48 _i_49 _j_50) _partial-inst_51)
        ((helix3* _nuc_48 _i_49 _j_50) _partial-inst_51)))))
 (define P-O3*
   (lambda (_nucs_48 _i_49 _j_50)
     (lambda (_partial-inst_51)
       ((lambda (_ref_52)
          ((lambda (_align_53)
             ((letrec ((_loop_54
                        (lambda (_lst_55 _domains_56)
                          (if (null? _lst_55)
                            _domains_56
                            ((lambda (_nuc_57)
                               ((lambda (_tfo-60_58 _tfo-180_59 _tfo-275_60)
                                  (_loop_54
                                   (cdr _lst_55)
                                   (cons
                                    (vector _i_49 _tfo-60_58 _nuc_57)
                                    (cons
                                     (vector _i_49 _tfo-180_59 _nuc_57)
                                     (cons
                                      (vector _i_49 _tfo-275_60 _nuc_57)
                                      _domains_56)))))
                                (tfo-combine (vector-ref _nuc_57 4) _align_53)
                                (tfo-combine (vector-ref _nuc_57 3) _align_53)
                                (tfo-combine
                                 (vector-ref _nuc_57 2)
                                 _align_53)))
                             (car _lst_55))))))
                _loop_54)
              _nucs_48
              '()))
           (tfo-inv-ortho
            (tfo-align
             ((lambda (_v_53)
                (tfo-apply
                 (vector-ref _v_53 1)
                 (vector-ref (vector-ref _v_53 2) 23)))
              _ref_52)
             ((lambda (_v_53)
                (tfo-apply
                 (vector-ref _v_53 1)
                 (vector-ref (vector-ref _v_53 2) 21)))
              _ref_52)
             ((lambda (_v_53)
                (tfo-apply
                 (vector-ref _v_53 1)
                 (vector-ref (vector-ref _v_53 2) 12)))
              _ref_52)))))
        (get-var _j_50 _partial-inst_51)))))
 (define anticodon-constraint?
   (lambda (_v_48 _partial-inst_49)
     (if (= (vector-ref _v_48 0) 33)
       ((lambda (_p_50 _o3*_51) (<= (pt-dist _p_50 _o3*_51) 3.0))
        ((lambda (_v_50)
           (tfo-apply
            (vector-ref _v_50 1)
            (vector-ref (vector-ref _v_50 2) 5)))
         (get-var 34 _partial-inst_49))
        ((lambda (_v_50)
           (tfo-apply
            (vector-ref _v_50 1)
            (vector-ref (vector-ref _v_50 2) 23)))
         _v_48))
       #t)))
 (define anticodon
   (lambda (_rAs_48 _rA_49 _rC_50 _rCs_51 _rG_52 _rGs_53 _rU_54 _rUs_55)
     (letrec ((_anticodon-domains_56
               (list
                (reference _rC_50 27)
                (helix5* _rC_50 28 27)
                (helix5* _rA_49 29 28)
                (helix5* _rG_52 30 29)
                (helix5* _rA_49 31 30)
                (wc _rU_54 39 31)
                (helix5* _rC_50 40 39)
                (helix5* _rU_54 41 40)
                (helix5* _rG_52 42 41)
                (helix5* _rG_52 43 42)
                (stacked3* _rA_49 38 39)
                (stacked3* _rG_52 37 38)
                (stacked3* _rA_49 36 37)
                (stacked3* _rA_49 35 36)
                (stacked3* _rG_52 34 35)
                (P-O3* _rCs_51 32 31)
                (P-O3* _rUs_55 33 32))))
       (search '() _anticodon-domains_56 anticodon-constraint?))))
 (define pseudoknot-constraint?
   (lambda (_v_48 _partial-inst_49)
     ((lambda (_key_50)
        (if (if (_eqv?_15 _key_50 '18) #t #f)
          ((lambda (_p_51 _o3*_52) (<= (pt-dist _p_51 _o3*_52) 4.0))
           ((lambda (_v_51)
              (tfo-apply
               (vector-ref _v_51 1)
               (vector-ref (vector-ref _v_51 2) 5)))
            (get-var 19 _partial-inst_49))
           ((lambda (_v_51)
              (tfo-apply
               (vector-ref _v_51 1)
               (vector-ref (vector-ref _v_51 2) 23)))
            _v_48))
          (if (if (_eqv?_15 _key_50 '6) #t #f)
            ((lambda (_p_51 _o3*_52) (<= (pt-dist _p_51 _o3*_52) 4.5))
             ((lambda (_v_51)
                (tfo-apply
                 (vector-ref _v_51 1)
                 (vector-ref (vector-ref _v_51 2) 5)))
              (get-var 7 _partial-inst_49))
             ((lambda (_v_51)
                (tfo-apply
                 (vector-ref _v_51 1)
                 (vector-ref (vector-ref _v_51 2) 23)))
              _v_48))
            #t)))
      (vector-ref _v_48 0))))
 (define pseudoknot
   (lambda (_rA_48
            _rAs_49
            _rC_50
            _rCs_51
            _rG_52
            _rGs_53
            _rU_54
            _rUs_55
            _rG*_56
            _rU*_57)
     (letrec ((_pseudoknot-domains_58
               (list
                (reference _rA_48 23)
                (wc-Dumas _rU_54 8 23)
                (helix3* _rG_52 22 23)
                (wc-Dumas _rC_50 9 22)
                (helix3* _rG_52 21 22)
                (wc-Dumas _rC_50 10 21)
                (helix3* _rC_50 20 21)
                (wc-Dumas _rG_52 11 20)
                (helix3* _rU*_57 19 20)
                (wc-Dumas _rA_48 12 19)
                (helix3* _rC_50 3 19)
                (wc-Dumas _rG_52 13 3)
                (helix3* _rC_50 2 3)
                (wc-Dumas _rG_52 14 2)
                (helix3* _rC_50 1 2)
                (wc-Dumas _rG*_56 15 1)
                (P-O3* _rUs_55 16 15)
                (P-O3* _rCs_51 17 16)
                (P-O3* _rAs_49 18 17)
                (helix3* _rU_54 7 8)
                (P-O3* _rCs_51 4 3)
                (stacked5* _rU_54 5 4)
                (stacked5* _rC_50 6 5))))
       (search '() _pseudoknot-domains_58 pseudoknot-constraint?))))
 (define list-of-atoms
   (lambda (_n_48)
     (append (list-of-common-atoms _n_48) (list-of-specific-atoms _n_48))))
 (define list-of-common-atoms
   (lambda (_n_48)
     (list
      (vector-ref _n_48 5)
      (vector-ref _n_48 6)
      (vector-ref _n_48 7)
      (vector-ref _n_48 8)
      (vector-ref _n_48 9)
      (vector-ref _n_48 10)
      (vector-ref _n_48 11)
      (vector-ref _n_48 12)
      (vector-ref _n_48 13)
      (vector-ref _n_48 14)
      (vector-ref _n_48 15)
      (vector-ref _n_48 16)
      (vector-ref _n_48 17)
      (vector-ref _n_48 18)
      (vector-ref _n_48 19)
      (vector-ref _n_48 20)
      (vector-ref _n_48 21)
      (vector-ref _n_48 22)
      (vector-ref _n_48 23)
      (vector-ref _n_48 24)
      (vector-ref _n_48 25)
      (vector-ref _n_48 26)
      (vector-ref _n_48 27)
      (vector-ref _n_48 28)
      (vector-ref _n_48 29))))
 (define list-of-specific-atoms
   (lambda (_n_48)
     (if (if (vector? _n_48) (eq? (vector-ref _n_48 0) 'rA) #f)
       (list
        (vector-ref _n_48 30)
        (vector-ref _n_48 31)
        (vector-ref _n_48 32)
        (vector-ref _n_48 33)
        (vector-ref _n_48 34)
        (vector-ref _n_48 35)
        (vector-ref _n_48 36)
        (vector-ref _n_48 37))
       (if (if (vector? _n_48) (eq? (vector-ref _n_48 0) 'rC) #f)
         (list
          (vector-ref _n_48 30)
          (vector-ref _n_48 31)
          (vector-ref _n_48 32)
          (vector-ref _n_48 33)
          (vector-ref _n_48 34)
          (vector-ref _n_48 35))
         (if (if (vector? _n_48) (eq? (vector-ref _n_48 0) 'rG) #f)
           (list
            (vector-ref _n_48 30)
            (vector-ref _n_48 31)
            (vector-ref _n_48 32)
            (vector-ref _n_48 33)
            (vector-ref _n_48 34)
            (vector-ref _n_48 35)
            (vector-ref _n_48 36)
            (vector-ref _n_48 37)
            (vector-ref _n_48 38))
           (list
            (vector-ref _n_48 30)
            (vector-ref _n_48 31)
            (vector-ref _n_48 32)
            (vector-ref _n_48 33)
            (vector-ref _n_48 34)))))))
 (define var-most-distant-atom
   (lambda (_v_48)
     (letrec ((_distance_49
               (lambda (_pos_50)
                 ((lambda (_abs-pos_51)
                    ((lambda (_x_52 _y_53 _z_54)
                       (sqrt
                        (+ (* _x_52 _x_52) (* _y_53 _y_53) (* _z_54 _z_54))))
                     (vector-ref _abs-pos_51 0)
                     (vector-ref _abs-pos_51 1)
                     (vector-ref _abs-pos_51 2)))
                  (tfo-apply (vector-ref _v_48 1) _pos_50)))))
       (maximum (map _distance_49 (list-of-atoms (vector-ref _v_48 2)))))))
 (define sol-most-distant-atom
   (lambda (_s_48) (maximum (map var-most-distant-atom _s_48))))
 (define most-distant-atom
   (lambda (_sols_48) (maximum (map sol-most-distant-atom _sols_48))))
 (define maximum
   (lambda (_lst_48)
     ((letrec ((_loop_49
                (lambda (_m_50 _l_51)
                  (if (null? _l_51)
                    _m_50
                    ((lambda (_x_52)
                       (_loop_49 (if (> _x_52 _m_50) _x_52 _m_50) (cdr _l_51)))
                     (car _l_51))))))
        _loop_49)
      (car _lst_48)
      (cdr _lst_48))))
 (define check
   (lambda (_rA_48
            _rAs_49
            _rC_50
            _rCs_51
            _rG_52
            _rGs_53
            _rU_54
            _rUs_55
            _rG*_56
            _rU*_57)
     (length
      (pseudoknot
       _rA_48
       _rAs_49
       _rC_50
       _rCs_51
       _rG_52
       _rGs_53
       _rU_54
       _rUs_55
       _rG*_56
       _rU*_57))))
 (define run
   (lambda (_rA_48
            _rAs_49
            _rC_50
            _rCs_51
            _rG_52
            _rGs_53
            _rU_54
            _rUs_55
            _rG*_56
            _rU*_57)
     (most-distant-atom
      (pseudoknot
       _rA_48
       _rAs_49
       _rC_50
       _rCs_51
       _rG_52
       _rGs_53
       _rU_54
       _rUs_55
       _rG*_56
       _rU*_57))))
 (provide (contract-out
           (run
            (->
             rA/c
             (listof rA/c)
             rC/c
             (listof rC/c)
             rG/c
             (listof rG/c)
             rU/c
             (listof rU/c)
             rG/c
             rU/c
             any/c)))))

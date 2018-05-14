;Jesse Zamazanuk
;Various LISP functions constructed from basic functional forms

;Evaluates to the last element of list L.
;	eg.  (myLast ‘(p a e g))  →  g
(defun myLast (L)
  (if (eq (cdr L) nil )
    (car L)
    (myLast (cdr L))))

;Evaluates to the number of occurrences of atom X in list L.
; eg.  (myCount ‘a ‘(p k a t p a e g))  →  2
(defun myCount (X L)
  (if (eq X (car L))
    (+ 1 (myCount X (cdr L)))
    0))

;Evaluates to ‘true’ if X is an atom in list L, ‘false’ otherwise.
;	eg.  (myMember ‘a ‘(p a e g))  →  t
(defun myMember (X L)
  (cond ((eq L nil) nil)
  ((eq X (car L))
    t)
  ((not(eq X (car L)))
    (myMember X (cdr L))
    )))

;Evaluates to a list with all elements of  L without repetition.
; eg.  (myPurge ‘(p a c e p c))  →  (a e p c)
(defun myPurge (L)
    (cond ((eq L nil) nil)
      ((myMember (car L) (cdr L))
      (myPurge (cdr L)))
      (t (cons (car L) (myPurge (cdr L))))))

;Evaluates to a list of elements that are common in both lists L1 and L2.
;	Assume L1 and L2 have no repeated elements.
;	eg.  (myCommon ‘(p a e g) ‘(a q r e))  →  (a e)
(defun myCommon (L1 L2)
    (cond ((eq L1 nil) L1)
          ((myMember (car L1) L2)
            (cons (car L1) (myCommon (cdr L1) L2)))
            ((not(myMember (car L1) L2))
            (myCommon (cdr L1) L2))))

;Given integers X and Y, evaluates to the list of increasing integers between X and Y inclusive. ( or to  nil  if such list does not exist )
; eg.  (myGen 3 11)  →  (3 4 5 6 7 8 9 10 11)
; eg.  (myGen 4 4)  →  (4)
; eg.  (myGen 11 3)  →  ()
(defun myGen (X Y)
    (if(< (- Y X) 0)
      nil
      (cons X (myGen (+ X 1) Y))))

;Evaluates to the list which results from applying function F to every element of list L.
;	eg.  (myMap (lambda (x) (* 2 x)) ‘(1 2 3 4) )  →  (2 4 6 8)
(defun myMap (F L)
    (if (eq L nil)
      nil
      (cons (funcall F (car L)) (myMap F (cdr L)))))

;Evaluates to the the results of applying aggregate function F to the elements of L.
; L will be of size >= 2.
; F will be a commutative function.
;	eg.  (myReduce (lambda (x y) (+ x y)) ‘(1 2 3 4 5))  →  15
(defun myReduce (F L)
    (if (eq (cdr L) nil)
      (car L)
      (myReduce F (cons (funcall F (car L) (car (cdr L))) (cdr (cdr L))))))

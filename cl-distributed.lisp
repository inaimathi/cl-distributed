;;;; cl-distributed.lisp

(in-package #:cl-distributed)

;;;;;;;;;; Classes, creation and copying
(defclass edit () ((ix :reader ix :initarg :ix)))
(defclass insertion (edit)
  ((len :reader len :initarg :len)
   (str :reader str :initarg :str)))
(defclass deletion (edit)
  ((ct :reader ct :initarg :ct)))

(defmethod mk-deletion ((ix integer) (ct integer))
  (make-instance 'deletion :ix ix :ct ct))
(defmethod new-deletion ((old deletion) &key ix ct)
  (make-instance 'deletion :ix (or ix (ix old)) :ct (or ct (ct old))))

(defmethod mk-insertion ((ix integer) (str string))
  (make-instance 'insertion :ix ix :len (length str) :str str))
(defmethod new-insertion ((old insertion) &key ix str)
  (make-instance 
   'insertion
   :ix (or ix (ix old))
   :len (if str (length str) (len old))
   :str (or str (str old))))

;;;;;;;;;; Basic history methods
(defmethod apply-payload ((str string) (ins insertion))
  (if (zerop (ix ins))
      (concatenate 'string (str ins) str)
      (concatenate 'string 
       (subseq str 0 (ix ins)) 
       (str ins)
       (subseq str (ix ins)))))

(defmethod apply-payload ((str string) (del deletion))
  (if (zerop (ix del))
      (subseq str (ix del))
      (concatenate 'string 
	(subseq str 0 (ix del))
	(ignore-errors (subseq str (+ (ix del) (ct del)))))))

(defmethod reconcile ((a deletion) (b deletion))
  (cond ((eclipsed-by? a b) nil)
	((overlapped-at-start? a b)
	 (let ((delta (- (ct b) (- (ix a) (ix b)))))
	   (mk-deletion (ix b) (- (ct a) delta))))
	((overlapped-at-end? a b)
	 (new-deletion a :ct (- (ix b) (ix a))))
	((and (> (ix b) (ix a)) ;; overlaps
	      (> (+ (ix a) (ct a)) (ix b)))
	 (new-deletion a :ct (- (ct a) (ct b))))
	((irrelevant? a b) a)
	(t (bump a (- (ct b))))))

(defmethod reconcile ((a deletion) (b insertion))
  (cond ((> (ix b) (+ (ix a) (ct a)))
	 a)
	((>= (ix a) (ix b))
	 (bump a (len b)))
	(t
	 (new-deletion a :ct (+ (ct a) (len b))))))

(defmethod reconcile ((a insertion) (b deletion))
  (cond ((eclipsed-by? a b) nil)
	((>= (ix a) (ix b))
	 (bump a (- (ct b))))
	(t a)))

(defmethod reconcile ((a edit) (b insertion))
  (if (>= (ix a) (ix b))
      (bump a (len b))
      a))

(defmethod bump ((ins insertion) (delta integer))
  (new-insertion ins :ix (+ (ix ins) delta)))
(defmethod bump ((del deletion) (delta integer))
  (new-deletion del :ix (+ (ix del) delta)))

;; (update! *arc* 0 (mk-insertion 0 "Testing"))
;; (update! *arc* 0 (mk-insertion 0 "Testicles"))
;; (update! *arc* 2 (mk-deletion 4 3))
;; (update! *arc* 2 (mk-insertion 7 " "))
;; (update! *arc* 2 (mk-deletion 11 5))

;; projects to "Test Test"

;;;;;;;;;; Minor internal utility
(defmethod eclipsed-by? ((a deletion) (b deletion))
  (>= (+ (ix b) (ct b)) (+ (ix a) (ct a))
      (ix a) (ix b)))
(defmethod eclipsed-by? ((a insertion) (b deletion))
  (> (+ (ix b) (ct b)) (ix a) (ix b)))

(defmethod overlapped-at-start? ((a deletion) (b deletion))
  (and (>= (ix a) (ix b))
       (> (+ (ix b) (ct b)) (ix a))))

(defmethod overlapped-at-end? ((a deletion) (b deletion))
  (and (> (ix b) (ix a))
       (> (+ (ix a) (ct a)) (ix b))
       (> (+ (ix b) (ct b)) (+ (ix a) (ct a)))))

(defmethod irrelevant? ((a deletion) (b edit))
  (>= (ix b) (+ (ix a) (ct a))))

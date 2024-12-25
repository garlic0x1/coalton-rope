(defpackage #:coal-rope
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:string #:coalton-library/string)
   (#:math #:coalton-library/math))
  (:export
   #:Rope
   #:splice))
(in-package #:coal-rope)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;------------;;
  ;; Parameters ;;
  ;;------------;;

  (declare *short-leaf* UFix)
  (define *short-leaf* 16)

  (declare *long-leaf* UFix)
  (define *long-leaf* 16)

  ;;-------;;
  ;; Types ;;
  ;;-------;;

  (define-struct Rope
    (length UFix)
    (depth UFix)
    (node Node))

  (define-type Node
    (Branch Rope Rope)
    (Leaf String))

  ;;-------;;
  ;; Utils ;;
  ;;-------;;

  (declare collect (Rope -> (List String)))
  (define (collect rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf str) (make-list str))
      ((Branch l r) (append (collect l) (collect r)))))

  (declare weight (Rope -> UFix))
  (define (weight rope)
    (let (Rope length _ node) = rope)
    (match node
      ((Leaf _) length)
      ((Branch (Rope length _ _) _) length)))

  (declare make-leaf (String -> Rope))
  (define (make-leaf str)
    (Rope (into (string:length str)) 0 (Leaf str)))

  ;;--------;;
  ;; Traits ;;
  ;;--------;;

  (define-instance (Into String Rope)
    (define (into str)
      (let length = (string:length str))
      (if (<= *long-leaf* length)
          (progn
            (let (Tuple ante post) = (string:split (math:div length 2) str))
            (splice (into ante) (into post)))
          (Rope (into length) 0 (Leaf str)))))

  (define-instance (Into Rope String)
    (define (into rope)
      (fold (fn (acc el) (string:concat acc el)) "" (collect rope))))

  ;;-----------;;
  ;; Rotations ;;
  ;;-----------;;

  (declare rotate-l (Rope -> Rope))
  (define (rotate-l rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) rope)
      ((Branch l (Rope _ _ (Branch rl rr))) (splice* (splice l rl) rr))
      ((Branch _ _) rope)))

  (declare rotate-r (Rope -> Rope))
  (define (rotate-r rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) rope)
      ((Branch (Rope _ _ (Branch ll lr)) r) (splice* ll (splice lr r)))
      ((Branch _ _) rope)))

  (declare rotate-lr (Rope -> Rope))
  (define (rotate-lr rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) rope)
      ((Branch l r) (rotate-r (splice* (rotate-l l) r)))))

  (declare rotate-rl (Rope -> Rope))
  (define (rotate-rl rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) rope)
      ((Branch l r) (rotate-l (splice* l (rotate-r r))))))

  ;;-------------;;
  ;; AVL Balance ;;
  ;;-------------;;

  (declare balance-factor (Rope -> IFix))
  (define (balance-factor rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) 0)
      ((Branch (Rope _ dl _) (Rope _ dr _)) (into (- dl dr)))))

  (declare balance (Rope -> Rope))
  (define (balance rope)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf _) rope)
      ((Branch l r)
       (let bf = (balance-factor rope))
       (cond ((< 1 bf)
              (balance
               (if (negative? (balance-factor l))
                   (rotate-lr rope)
                   (rotate-r rope))))
             ((> -1 bf)
              (balance
               (if (positive? (balance-factor r))
                   (rotate-rl rope)
                   (rotate-l rope))))
             (True rope)))))

  ;;-------------;;
  ;; Concatenate ;;
  ;;-------------;;

  (declare splice* (Rope -> Rope -> Rope))
  (define (splice* l r)
    (let (Rope length-l depth-l _) = l)
    (let (Rope length-r depth-r _) = r)
    (Rope (+ length-l length-r)
          (1+ (max depth-l depth-r))
          (Branch l r)))

  (declare splice (Rope -> Rope -> Rope))
  (define (splice l r)
    (balance (splice* l r)))

  ;;-------;;
  ;; Split ;;
  ;;-------;;

  (declare cut (Rope -> Ufix -> (Tuple Rope Rope)))
  (define (cut rope index)
    (let (Rope _ _ node) = rope)
    (match node
      ((Leaf str)
       (let (Tuple ante post) = (string:split index str))
       (Tuple (make-leaf ante) (make-leaf post)))
      ((Branch l r)
       (let ((w (weight rope)))
         (cond ((== w index)
                (Tuple l r))
               ((< index w)
                (let (Tuple ante post) = (cut l index))
                (Tuple (balance ante) (splice post r)))
               ((> index w)
                (let (Tuple ante post) = (cut r (- index w)))
                (Tuple (splice l ante) (balance post))))))))

  ;;--------;;
  ;; Insert ;;
  ;;--------;;

  (declare insert ((Into :a Rope) => Rope -> UFix -> :a -> Rope))
  (define (insert rope index obj)
    (let (Tuple ante post) = (cut rope index))
    (splice (splice ante (into obj)) post))
  )

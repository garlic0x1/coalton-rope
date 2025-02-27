(defpackage #:coalton-rope
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames
   (#:string #:coalton-library/string)
   (#:list #:coalton-library/list)
   (#:math #:coalton-library/math)
   (#:iter #:coalton-library/iterator))
  (:shadow
   #:append
   #:length
   #:reverse)
  (:export
   #:Rope
   #:new
   #:length
   #:reverse
   #:cut
   #:subrope
   #:splice
   #:prepend
   #:append
   #:insert
   #:ref-unchecked
   #:ref
   #:starts-with?
   #:subrope?
   #:subrope-index
   ))
(in-package #:coalton-rope)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;------------;;
  ;; Parameters ;;
  ;;------------;;

  (declare *short-leaf* UFix)
  (define *short-leaf* 16)

  (declare *long-leaf* UFix)
  (define *long-leaf* 128)

  ;;-------;;
  ;; Types ;;
  ;;-------;;

  (define-struct Dimensions
    (length UFix)
    (depth UFix))

  (define-type Rope
    (Branch Dimensions Rope Rope)
    (Leaf Dimensions String))

  ;;-------;;
  ;; Utils ;;
  ;;-------;;

  (declare nextn! (UFix -> Iterator :elt -> (List :elt)))
  (define (nextn! count iter)
    (if (zero? count)
        Nil
        (match (iter:next! iter)
          ((Some item) (Cons item (nextn! (1- count) iter)))
          ((None) Nil))))

  (declare rope-dimensions (Rope -> Dimensions))
  (define (rope-dimensions rope)
    (match rope
      ((Leaf dim _) dim)
      ((Branch dim _ _) dim)))

  (declare short-leaf? ((Into :a Rope) => Rope -> :a -> Boolean))
  (define (short-leaf? leaf obj)
    (match leaf
      ((Branch _ _ _) False)
      ((Leaf (Dimensions length _) _)
       (>= *short-leaf* (+ length (.length (rope-dimensions (into obj))))))))

  (declare make-leaf (String -> Rope))
  (define (make-leaf str)
    (Leaf (Dimensions (into (string:length str)) 0) str))

  (declare collect (Rope -> (List String)))
  (define (collect rope)
    (match rope
      ((Leaf _ str) (make-list str))
      ((Branch _ l r) (list:append (collect l) (collect r)))))

  (declare weight (Rope -> UFix))
  (define (weight rope)
    (match rope
      ((Leaf (Dimensions length _) _) length)
      ((Branch _ l _) (.length (rope-dimensions l)))))

  (declare new (Unit -> Rope))
  (define (new)
    (the Rope mempty))

  (declare length (Rope -> UFix))
  (define (length rope)
    (.length (rope-dimensions rope)))

  (declare reverse (Rope -> Rope))
  (define (reverse rope)
    (let leaves = (collect rope))
    (fold (fn (acc leaf) (splice (make-leaf (string:reverse leaf)) acc))
          (new)
          leaves))

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
          (Leaf (Dimensions length 0) str))))

  (define-instance (Into Rope String)
    (define (into rope)
      (fold (fn (acc el) (string:concat acc el)) "" (collect rope))))

  (define-instance (Eq Rope)
    (define (== a b)
      (or (lisp Boolean (a b) (to-boolean (cl:eq a b)))
          (iter:elementwise==! (iter:into-iter a) (iter:into-iter b)))))

  (define-instance (Semigroup Rope)
    (define <> splice))

  (define-instance (Monoid Rope)
    (define mempty (Leaf (Dimensions 0 0) "")))

  (define-instance (iter:IntoIterator Rope Char)
    (define (iter:into-iter rope)
      (match rope
        ((Leaf _ str)
         (string:chars str))
        ((Branch _ l r)
         (iter:chain! (iter:into-iter l) (iter:into-iter r))))))

  (define-instance (iter:FromIterator Rope Char)
    (define (iter:collect! iter)
      (let str = (the String (into (nextn! *short-leaf* iter))))
      (if (== 0 (coalton-library/string:length str))
          (into str)
          (splice (into str) (iter:collect! iter)))))

  ;; Is this a bug in iter?
  ;; COALTON-ROPE/TEST> (coalton
  ;;                     (let ((iter (iter:into-iter "asdf")))
  ;;                       (print (the String (iter:collect! (iter:take! 3 iter))))
  ;;                       (print (the String (iter:collect! iter)))))

  ;;-----------;;
  ;; Rotations ;;
  ;;-----------;;

  (declare rotate-l (Rope -> Rope))
  (define (rotate-l rope)
    (match rope
      ((Branch _ l (Branch _ rl rr)) (splice* (splice l rl) rr))
      (_ rope)))

  (declare rotate-r (Rope -> Rope))
  (define (rotate-r rope)
    (match rope
      ((Branch _ (Branch _ ll lr) r) (splice* ll (splice lr r)))
      (_ rope)))

  (declare rotate-lr (Rope -> Rope))
  (define (rotate-lr rope)
    (match rope
      ((Leaf _ _) rope)
      ((Branch _ l r) (rotate-r (splice* (rotate-l l) r)))))

  (declare rotate-rl (Rope -> Rope))
  (define (rotate-rl rope)
    (match rope
      ((Leaf _ _) rope)
      ((Branch _ l r) (rotate-l (splice* l (rotate-r r))))))

  ;;-------------;;
  ;; AVL Balance ;;
  ;;-------------;;

  (declare balance-factor (Rope -> IFix))
  (define (balance-factor rope)
    (match rope
      ((Leaf _ _) 0)
      ((Branch _ l r)
       (- (into (.depth (rope-dimensions l)))
          (into (.depth (rope-dimensions r)))))))

  (declare balance (Rope -> Rope))
  (define (balance rope)
    (match rope
      ((Leaf _ _) rope)
      ((Branch _ l r)
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
    (let (Dimensions length-l depth-l) = (rope-dimensions l))
    (let (Dimensions length-r depth-r) = (rope-dimensions r))
    (Branch (Dimensions (+ length-l length-r) (1+ (max depth-l depth-r)))
            l
            r))

  (declare splice (Rope -> Rope -> Rope))
  (define (splice l r)
    (balance (splice* l r)))

  ;;-------;;
  ;; Split ;;
  ;;-------;;

  (declare cut (Rope -> Ufix -> (Tuple Rope Rope)))
  (define (cut rope index)
    (match rope
      ((Leaf _ str)
       (let (Tuple ante post) = (string:split index str))
       (Tuple (make-leaf ante) (make-leaf post)))
      ((Branch _ l r)
       (let ((w (weight rope)))
         (cond ((== w index)
                (Tuple l r))
               ((< index w)
                (let (Tuple ante post) = (cut l index))
                (Tuple (balance ante) (splice post r)))
               ((> index w)
                (let (Tuple ante post) = (cut r (- index w)))
                (Tuple (splice l ante) (balance post))))))))

  (declare subrope (Rope -> Ufix -> Ufix -> Rope))
  (define (subrope rope start end)
    (let (Tuple ante _) = (cut rope end))
    (let (Tuple _ post) = (cut ante start))
    post)

  ;;--------;;
  ;; Insert ;;
  ;;--------;;

  (declare prepend (Rope -> Rope -> Rope))
  (define (prepend rope obj)
    (match (Tuple rope obj)
      ((Tuple (Leaf _ str-a) (Leaf _ str-b))
       (if (short-leaf? rope obj)
           (make-leaf (string:concat str-b str-a))
           (splice obj rope)))
      ((Tuple _ (Branch _ _ _))
       (splice obj rope))
      ((Tuple (Branch _ l r) _)
       (splice (prepend l obj) r))))

  (declare append (Rope -> Rope -> Rope))
  (define (append rope obj)
    (match (Tuple rope obj)
      ((Tuple (Leaf _ str-a) (Leaf _ str-b))
       (if (short-leaf? rope obj)
           (make-leaf (string:concat str-a str-b))
           (splice rope obj)))
      ((Tuple _ (Branch _ _ _))
       (splice rope obj))
      ((Tuple (Branch _ l r) _)
       (splice l (append r obj)))))

  (declare insert ((Into :a Rope) => Rope -> UFix -> :a -> Rope))
  (define (insert rope index obj)
    (let (Tuple ante post) = (cut rope index))
    (splice (append ante (into obj)) post))

  (declare ref-unchecked (Rope -> UFix -> Char))
  (define (ref-unchecked rope idx)
    (match rope
      ((Branch (Dimensions length _) l r)
       (if (< idx length)
           (ref-unchecked l idx)
           (ref-unchecked r (- idx length))))
      ((Leaf _ str)
       (string:ref-unchecked str idx))))

  (declare ref (Rope -> UFix -> (Optional Char)))
  (define (ref rope idx)
    (if (< idx (length rope))
        (Some (ref-unchecked rope idx))
        None))

  ;;--------;;
  ;; Search ;;
  ;;--------;;

  (declare starts-with? (Rope -> Rope -> Boolean))
  (define (starts-with? rope subrope)
    (== subrope (fst (cut rope (length subrope)))))

  (declare subrope-index (Rope -> Rope -> (Optional UFix)))
  (define (subrope-index rope subrope)
    (iter:find!
     (fn (i)
       (let (Tuple _ post) = (cut rope i))
       (starts-with? post subrope))
     (iter:up-to (length rope))))

  (declare subrope? (Rope -> Rope -> Boolean))
  (define (subrope? rope subrope)
    (some? (subrope-index rope subrope))))

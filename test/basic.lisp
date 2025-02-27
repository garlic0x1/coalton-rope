(in-package #:coalton-rope/test)

(define-test rope-string-conversions ()
  (is (== "asdf" (into (rope:splice (rope:new) (into "asdf"))))))

(define-test rope-splicing ()
  (is (== "asdfuiop" (into (rope:splice (into "asdf") (into "uiop")))))
  (is (== (the UFix 1) (.depth (rope::rope-dimensions (rope:splice (into "asdf") (into "uiop"))))))
  (is (== (the UFix 0) (.depth (rope::rope-dimensions (rope:append (into "asdf") (into "uiop")))))))

(define-test rope-collect ()
  (let ((iter (iter:into-iter "asdfuiop"))
        (rope (the rope:Rope (iter:collect! iter)))
        (dims (rope::rope-dimensions rope)))
    (is (== (the UFix 8) (.length dims)))
    (is (== (the UFix 1) (.depth dims))))

  (let ((iter (iter:repeat-for #\a 128))
        (rope (the rope:Rope (iter:collect! iter)))
        (dims (rope::rope-dimensions rope)))
    (is (== (the UFix 128) (.length dims)))
    (is (== (the UFix 4) (.depth dims)))))

(define-test rope-cut ()
  (let rope = (into "coalton is awesome!"))
  (let (Tuple ante post) = (rope:cut rope 4))
  (is (== "coal" (into ante)))
  (let (Tuple ante post) = (rope:cut rope 100))
  (is (== "" (into post)))
  (let (Tuple ante post) = (rope:cut rope 8))
  (is (== "is awesome!" (into post))))

(define-test rope-subrope ()
  (let rope = (into "this is a rope"))
  (is (== "is" (into (rope:subrope rope 5 7))))
  (is (== "" (into (rope:subrope rope 0 0))))
  (is (== "" (into (rope:subrope rope 7 5))))
  (is (== "" (into (rope:subrope rope 100 102)))))

;; TODO idk if we want this slop
(define-test rope-insert ()
  (let rope = (the rope:Rope (into "asdfuiop")))
  (is (== (the rope:Rope (into "asdfhjkluiop"))
          (rope:insert rope 4 "hjkl")))
  (is (== (the rope:Rope (into "asdfuiophjkl"))
          (rope:insert rope 100 "hjkl"))))

(define-test rope-eq ()
  ;; same address
  (let a = (the rope:Rope (into "asdfuiop")))
  (let b = a)
  (is (== a b))
  ;; not same address but same string
  (let a = (rope:splice (rope:new) (into "asdf")))
  (let b = (rope:splice (into "asdf") (rope:new)))
  (is (== a b))
  ;; make sure we don't cut off the end
  (let a = (the rope:Rope (into "asdfa")))
  (let b = (the rope:Rope (into "asdf")))
  (is (not (== a b))))

(define-test rope-reverse ()
  (let rope = (the rope:Rope (into "asdfuiop")))
  (is (== "poiufdsa" (into (rope::reverse rope)))))

(define-test rope-ref ()
  (let rope = (the rope:Rope (into "asdfuiop")))
  (is (== #\a (rope:ref-unchecked rope 0)))
  (is (== #\p (rope:ref-unchecked rope 7)))
  (is (== None (rope:ref rope 10))))

(define-test rope-subrope? ()
  (is (rope:subrope? (into "asdfuiop") (into "uiop")))
  (is (not (rope:subrope? (into "asdfuiop") (into "uiopi")))))

(define-test rope-starts-with? ()
  (let a = (the rope:Rope (into "asdfasdf")))
  (is (rope:starts-with? a (into "asdfasdf")))
  (is (rope:starts-with? a (into "asdf")))
  (is (not (rope:starts-with? a (into "asdfasdfasdf"))))
  (is (not (rope:starts-with? a (into "asdfuiop")))))

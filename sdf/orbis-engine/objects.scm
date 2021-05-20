#| -*-Scheme-*-

This file defines the object types for Orbis. Properties are specified and accessed using the procedures defined here.

|#

(define numeric-vector
  (vector-constructor number?))
;;; Object types for Orbis Engine

(define thing:session
  (make-property 'session
                 'predicate (lambda (x) (orbis-session? x))))

(define thing:position
  (make-property 'position
                 'predicate n:vector?
                 'default-value 'vector 0 0))

(define thing:velocity
  (make-property 'velocity
                 'predicate n:vector?
                 'default-value 'vector 0 0))

(define thing:acceleration
  (make-property 'acceleration
                 'predicate n:vector?
                 'default-value 'vector 0 0))

(define thing:mass
  (make-property 'mass
                 'predicate n:exact-integer?
                 'default-value 0))
(define thing?
  (make-type 'thing (list thing:session thing:position thing:velocity thing:acceleration thing:mass)))
(set-predicate<=! thing? object?)

(define make-thing
  (type-instantiator thing?))

(define get-session
  (property-getter thing:session thing?))

(define get-position
  (property-getter thing:position thing?))

(define get-velocity
  (property-getter thing:velocity thing?))

(define get-acceleration
  (property-getter thing:acceleration thing?))

(define get-mass
  (property-getter thing:mass thing?))

(define set-position!
  (property-setter thing:position thing? any-object?))

(define set-velocity!
  (property-setter thing:velocity thing? any-object?))

(define set-acceleration!
  (property-setter thing:acceleration thing? any-object?))

(define set-mass!
  (property-setter thing:mass thing? any-object?))

(define (distance-between-things pos1 pos2)
  (magnitude (- pos1 pos2)))
 
(define (get-resultant-vector v1 v2)
  (+ v1 v2))

(define (get-collision-velocity thing1 thing2)
  (get-velocity thing2))

(define (set-collision-velocities! thing1 thing2)
  (let ((v1 (get-collision-velocity thing1 thing2)))
    (let ((v2 (get-collision-velocity thing2 thing1)))
      (set-velocity! thing1 v1)
      (set-velocity! thing2 v2))))

(define (check-for-collisions-and-move! thing)
  (display "Checking for collisions...")
  (let ((count 0))
    (for-each (lambda (other-thing)
                (if (< (distance-between-things (get-position other-thing) (get-position thing)) (magnitude (numeric-vector 1 1)))
                    (set! count (+ count 1)))
                (newline)
                (if (> count 1)
                    (set-collision-velocities! thing other-thing)))
              (get-things (get-session thing))))

  (set-position! thing (get-resultant-vector (get-position thing) (get-velocity thing)))
  (set-velocity! thing (get-resultant-vector (get-velocity thing) (get-acceleration thing))))
      
      
(define-generic-procedure-handler set-up! (match-args thing?)
  (lambda (super thing)
    (super thing)
    (add-thing! (get-session thing) thing)
    (register-with-clock! thing (get-clock))))

(define-generic-procedure-handler tear-down! (match-args thing?)
  (lambda (super thing)
    (remove-thing! (get-session thing) thing)
    (unregister-with-clock! thing (get-clock))
    (super thing)))

(define-clock-handler thing? check-for-collisions-and-move!)




;;; Action-at-a-Distance Forces

(define applied-force:session
  (make-property 'session
                 'predicate (lambda (x) (orbis-session? x))))

(define applied-force:newtons
  (make-property 'newtons
                 'predicate n:vector?
                 'default-value 'vector 0 0))

(define applied-force:concerned-things
  (make-property 'concerned-things
                 'predicate (is-list-of thing?)
                 'default-value '()))

(define applied-force?
  (make-type 'applied-force (list applied-force:session applied-force:newtons applied-force:concerned-things)))
(set-predicate<=! applied-force? object?)

(define make-applied-force
  (type-instantiator applied-force?))

(define get-force-session
  (property-getter applied-force:session applied-force?))

(define get-newtons
  (property-getter applied-force:newtons applied-force?))

(define get-concerned-things
  (property-getter applied-force:concerned-things applied-force?))

(define set-newtons!
  (property-setter applied-force:newtons applied-force? any-object?))

(define set-concerned-things!
  (property-setter applied-force:concerned-things applied-force? (is-list-of thing?)))

(define (get-resultant-acceleration thing f)
  (if (not (eqv? 0 (get-mass thing)))
      (* (get-newtons f) (get-mass thing))
      (get-acceleration thing)))

(define (apply-force! force)
  (for-each (lambda (a-thing)
              (set-acceleration! a-thing (get-resultant-acceleration a-thing force)))
            (get-concerned-things force)))

(define-generic-procedure-handler set-up! (match-args applied-force?)
  (lambda (super applied-force)
    (super applied-force)
    (add-applied-force! (get-force-session applied-force) applied-force)
    (register-with-clock! applied-force (get-clock))))

(define-generic-procedure-handler tear-down! (match-args applied-force?)
  (lambda (super applied-force)
    (remove-applied-force! (get-force-session applied-force) applied-force)
    (unregister-with-clock! applied-force (get-clock))
    (super applied-force)))

(define-clock-handler applied-force? apply-force!)


;;; Session Details

(define orbis-session:things
  (make-property 'things
                 'predicate (is-list-of thing?)
                 'default-value '()))

(define orbis-session:applied-forces
  (make-property 'applied-forces
                 'predicate (is-list-of applied-force?)
                 'default-value '()))

(define orbis-session?
  (make-type 'orbis-session (list orbis-session:things orbis-session:applied-forces)))
(set-predicate<=! orbis-session? object?)

(define make-orbis-session
  (type-instantiator orbis-session?))

(define get-things
  (property-getter orbis-session:things orbis-session?))

(define get-forces
  (property-getter orbis-session:applied-forces orbis-session?))

(define add-thing!
  (property-adder orbis-session:things orbis-session? thing?))

(define add-applied-force!
  (property-adder orbis-session:applied-forces orbis-session? applied-force?))

(define remove-thing!
  (property-remover orbis-session:things orbis-session? thing?))

(define remove-applied-force!
  (property-remover orbis-session:applied-forces orbis-session? applied-force?))

(define-generic-procedure-handler set-up!
  (match-args orbis-session?)
  (lambda (super orbis-session)
    (super orbis-session)
    (register-with-clock! orbis-session (get-clock))))

(define-generic-procedure-handler tear-down!
  (match-args orbis-session?)
  (lambda (super orbis-session)
    (unregister-with-clock! orbis-session (get-clock))
    (super avatar)))

(define (report-world-state! session)
    (for-each (lambda (thing)
                (newline)
                (display (get-name thing))
                (newline)
                (display (list "Position:" (get-position thing)))
                (newline)
                (display (list "Velocity:" (get-velocity thing)))
                (newline)
                (display (list "Acceleration:" (get-acceleration thing)))
                (newline)
                (display (list "Mass:" (get-mass thing)))
                (newline))
              (get-things session))
    (for-each (lambda (applied-force)
                (newline)
                (display (get-name applied-force))
                (newline)
                (display (list "Newtons:" (get-newtons applied-force)))
                (newline))
              (get-forces session)))

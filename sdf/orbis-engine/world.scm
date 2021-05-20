#| -*-Scheme-*-

This file relies on several dependencies from SDF by Chris Hanson and Gerald Jay Sussman. It specifies the setup of object types for Orbis.

|#

;;;; An adventure game at MIT

(define the-clock)
(define orbis-session)

(define (start-engine session-name size)
  (set! the-clock (make-clock))
  (set! orbis-session (create-orbis-session session-name size))
  (create-some-things orbis-session)
  (create-gravity orbis-session)
  (display "Session has started!"))

(define (get-clock)
  the-clock)

(define (what-is-there)
  (get-things orbis-session))

(define (create-some-things session)
  (create-thing 'example1 session (numeric-vector 1 1) (numeric-vector 2 2) (numeric-vector 0 0) 0)
  (create-thing 'test1 session (numeric-vector 1 1) (numeric-vector 6 6) (numeric-vector 0 0) 0))

(define (create-gravity session)
  (create-force 'gravity session (numeric-vector 0 10) (get-things session)))

(define (create-thing name orbis-session position velocity acceleration mass)
  (make-thing 'name name
              'session orbis-session
              'position position
              'velocity velocity
              'acceleration acceleration
              'mass mass))

(define (create-force name orbis-session newtons concerned-things)
  (make-applied-force 'name name
                      'session orbis-session
                      'newtons newtons
                      'concerned-things concerned-things))

(define (create-orbis-session name size)
  (make-orbis-session 'name name
                      'size size))

(define (report-session-state session)
  (report-world-state! session))

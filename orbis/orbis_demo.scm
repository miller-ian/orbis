
#| -*-Scheme-*-

Ian Miller -> 6.905 Final Project

ORBIS: A Modular Physics Simulation Engine

|#

(load "~/6945/sdf/manager/load")

(manage 'new 'orbis-engine)
(install-arithmetic! (make-vector-arithmetic numeric-arithmetic))

(start-engine 'Ian 10)

(report-session-state orbis-session)

(tick! (get-clock))
(report-session-state orbis-session)

(tick! (get-clock))
(report-session-state orbis-session)

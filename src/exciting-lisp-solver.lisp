(in-package :els)


;;;; Utilities ----------------------------------------------------------------

;;;; Reasoner -----------------------------------------------------------------
; (defun load-gdl-preamble (db)
;   (push-logic-frame-with db
;     (rule db (ggp-rules::not ?x) (call ?x) ! fail)
;     (fact db (ggp-rules::not ?x))

;     (rule db (ggp-rules::or ?x ?y) (call ?x))
;     (rule db (ggp-rules::or ?x ?y) (call ?y))

;     (rule db (ggp-rules::and ?x ?y) (call ?x) (call ?y))

;     (rule db (ggp-rules::distinct ?x ?x) ! fail)
;     (fact db (ggp-rules::distinct ?x ?y))))

; (defun make-reasoner-database ()
;   (let ((db (make-database)))
;     (load-gdl-preamble db)
;     db))


; (defclass prolog-reasoner ()
;   ((database :initform (make-reasoner-database) :reader pr-database)
;    (current-state :initform nil :accessor pr-state)
;    (current-moves :initform nil :accessor pr-moves)))

; (defun make-prolog-reasoner ()
;   (make-instance 'prolog-reasoner))

;;;; Players ------------------------------------------------------------------
(defclass stupid-player (ggp:ggp-player)
  ())


(defmethod ggp:player-start-game ((player stupid-player) rules role timeout)
  nil)

(defmethod ggp:player-stop-game ((player stupid-player))
  nil)

(defmethod ggp:player-update-game ((player stupid-player) moves)
  nil)

(defmethod ggp:player-select-move ((player stupid-player) timeout)
  'ggp-rules::im-dumb)

(defvar *stupid-player* (make-instance 'stupid-player
                                       :name "ELS-Stupid"
                                       :port 5000))

; (ggp:start-player *stupid-player*)
(ggp:kill-player *stupid-player*)

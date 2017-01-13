(in-package :els)


;;;; Utilities ----------------------------------------------------------------
(defun gdl-rule-p (form)
  (and (consp form)
       (eq (car form) 'ggp-rules::<=)))

(defun normalize-state (state)
  (remove-duplicates state :test #'equal))


;;;; Reasoner -----------------------------------------------------------------
(defun load-gdl-preamble (db)
  (push-logic-frame-with db
    (rule db (ggp-rules::not ?x) (call ?x) ! fail)
    (fact db (ggp-rules::not ?x))

    (rule db (ggp-rules::or ?x ?y) (call ?x))
    (rule db (ggp-rules::or ?x ?y) (call ?y))

    (rule db (ggp-rules::and ?x ?y) (call ?x) (call ?y))

    (rule db (ggp-rules::distinct ?x ?x) ! fail)
    (fact db (ggp-rules::distinct ?x ?y))))

(defun make-reasoner-database ()
  (let ((db (temperance:make-database)))
    (load-gdl-preamble db)
    db))


(defclass reasoner ()
  ((database :initform (make-reasoner-database) :reader reasoner-database)
   (current-state :initform nil :accessor reasoner-state)
   (current-moves :initform nil :accessor reasoner-moves)))

(defun make-reasoner ()
  (make-instance 'reasoner))


(defun clean-gdl (rules)
  ;; todo this
  rules)

(defun load-rule (rule)
  (if (gdl-rule-p rule)
    (apply #'invoke-rule t (rest rule))
    (invoke-fact t rule)))
(defun load-rules-into-reasoner (reasoner rules)
  (with-database (reasoner-database reasoner)
    (push-logic-frame-with t
      (map nil #'load-rule rules))))


(defun apply-state (reasoner state)
  (push-logic-frame-with t
    (iterate (for fact :in state)
             (invoke-fact t `(ggp-rules::true ,fact))))
  (setf (reasoner-state reasoner) state))

(defun apply-moves (reasoner moves)
  (push-logic-frame-with t
    (iterate (for (role . action) :in moves)
             (invoke-fact t `(ggp-rules::does ,role ,action))))
  (setf (reasoner-moves reasoner) moves))


(defun clear-state (reasoner)
  (pop-logic-frame (reasoner-database reasoner))
  (setf (reasoner-state reasoner) nil))

(defun clear-moves (reasoner)
  (pop-logic-frame (reasoner-database reasoner))
  (setf (reasoner-moves reasoner) nil))


(defun ensure-state (reasoner state)
  (when (not (eql state (reasoner-state reasoner)))
    (when (not (null (reasoner-moves reasoner)))
      (clear-moves reasoner))
    (when (not (null (reasoner-state reasoner)))
      (clear-state reasoner))
    (apply-state reasoner state)))

(defun ensure-moves (reasoner moves)
  (when (not (eql moves (reasoner-moves reasoner)))
    (when (not (null (reasoner-moves reasoner)))
      (clear-moves reasoner))
    (apply-moves reasoner moves)))


(defun initial-state (reasoner)
  (normalize-state
    (query-for (reasoner-database reasoner) ?what
               (ggp-rules::init ?what))))

(defun next-state (reasoner state moves)
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (ensure-moves reasoner moves)
    (normalize-state
      (query-for t ?what (ggp-rules::next ?what)))))


(defun legal-moves (reasoner state)
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (query-all t (ggp-rules::legal ?role ?action))))

(defun legal-moves-for (reasoner state role)
  (iterate (for move :in (legal-moves reasoner state))
           (when (eq (getf move '?role) role)
             (collect (getf move '?action)))))


;;;; Stupid Player ------------------------------------------------------------
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


;;;; Random Player ------------------------------------------------------------
(defclass random-player (ggp:ggp-player)
  ((role :accessor rp-role)
   (current-state :accessor rp-current-state)
   (reasoner :accessor rp-reasoner)))


(defmethod ggp:player-start-game ((player random-player) rules role timeout)
  (let ((reasoner (make-reasoner)))
    (setf (rp-role player) role
          (rp-reasoner player) reasoner)
    (load-rules-into-reasoner reasoner rules)
    (setf (rp-current-state player) (initial-state reasoner))))

(defmethod ggp:player-stop-game ((player random-player))
  (setf (rp-current-state player) nil
        (rp-reasoner player) nil
        (rp-role player) nil))

(defmethod ggp:player-update-game ((player random-player) moves)
  (when moves
    (zapf (rp-current-state player)
          (next-state (rp-reasoner player) % moves))))

(defmethod ggp:player-select-move ((player random-player) timeout)
  (random-elt (legal-moves-for (rp-reasoner player)
                               (rp-current-state player)
                               (rp-role player))))


(defvar *random-player* (make-instance 'random-player
                          :name "ELS-Random"
                          :port 5001))

(defvar *random-player2* (make-instance 'random-player
                          :name "ELS-Random2"
                          :port 5002))


;;;; Scratch ------------------------------------------------------------------
; (ggp:start-player *random-player2*)
; (ggp:kill-player *stupid-player*)

;;ONITAMA

;;Structures
(defstruct game win-state side-card red-player blue-player move-records active-player)

(defstruct player color pawns master current-cards active-card strategy)

(defstruct move current-pos final-pos)


;;Parameters


;;Need rule cards for:
;; Monkey, Crane, Mantis, Rabbit, Frog, Tiger, Dragon, Crab,
;; Elephant, Eel, Cobra, Horse, Ox, Rooster, Goose, Boar
;;
;;
(defparameter *horse* '((0 . 1) (0 . -1) (-1 . 0)))
(defparameter *ox* '((0 . 1) (0 . -1) (1 . 0)))
(defparameter *crane* '((0 . 1) (-1 . -1) (1 . -1)))
(defparameter *mantis* '((-1 . 1) (1 . 1) (0 . -1)))
(defparameter *eel* '((-1 . 1) (-1 . -1) (1 . 0)))
(defparameter *cobra* '((-1 . 0) (1 . 1) (1 . -1)))
(defparameter *rooster* '((-1 . -1) (-1 . 0) (1 . 0) (1 . 1)))
(defparameter *goose* '((-1 . 1) (-1 . 0) (1 . 0) (1 . -1)))
(defparameter *frog* '((-2 . 0) (-1 . 1) (1 . -1)))
(defparameter *rabbit* '((-1 . -1) (1 . 1) (2 . 0)))
(defparameter *monkey* '((-1 . 1) (1 . 1) (-1 . -1) (1 . -1)))
(defparameter *boar* '((1 . 0) (0 . -1) (0 . 1)))
(defparameter *tiger* '((2 . 0) (0 . -1)))
(defparameter *dragon* '((-1 . -1) (1 . -1) (-2 . 1) (2 . 1)))
(defparameter *crab* '((0 . 1) (-2 . 0) (2 . 0)))
(defparameter *elephant* '((-1 . 1) (-1 . 0) (1 . 0) (1 . 1)))

(defparameter *card-list* (list *horse* *ox* *crane* *mantis* *eel* *cobra* *rooster* *goose* 
                                         *frog* *rabbit* *monkey* *boar* *tiger* *dragon* *crab* *elephant*))

;;isMoveLegal function
(defun check-card-rule (original-pos final-pos card)

   (setf h-shift (- (car final-pos) (car original-pos)))
   (setf v-shift (- (cdr final-pos) (cdr original-pos)))

   (member nil card 
           :test (lambda (item card-rule) (and (eq h-shift (car card-rule)) (eq v-shift (cdr card-rule)))))
       
     
)

;;setup
(defun setup-game ()

(setf *shuffled-cards* (card-shuffle *card-list*))

(setf *player-1* 
    (make-player :color  'red
                 :pawns  '((1 . 1) (2 . 1) (4 . 1) (5 . 1))
                 :master '(3 . 1)
                 :current-cards (cons (first *shuffled-cards*) (second *shuffled-cards*))
                 :active-card *boar*
                 :strategy nil ))

(setf *player-2* 
      (make-player :color 'blue
                   :pawns  '((1 . 5) (2 . 5) (4 . 5) (5 . 5))
                   :master '(3 . 5)
                   :current-cards (cons (fourth *shuffled-cards*) (fifth *shuffled-cards*))
                   :active-card nil
                   :strategy nil))

(setf *game* 
      (make-game :win-state  nil
                 :side-card  (third *shuffled-cards*) ;;third
                 :red-player *player-1*
                 :blue-player *player-2*
                 :move-records nil
                 :active-player *player-1*))

(print *shuffled-cards*)
(print (player-current-cards *player-1*))

)

;;a function to check if your move is the winning move
;;(Win conditions: kill the other Master or put your Master on the opposite Throne)


;;Board Initialization


(defun card-shuffle (input-list)
  (loop with l = (length input-list)
        for i below l
        do (rotatef (nth i input-list)
                    (nth (random l) input-list)))
  input-list)

(defun print-board-state ()
  (princ "Game State")
  (princ "Blue \n")
  (princ "Master \n")
  (princ (player-master *player-1*))
  (princ "\nPawns: ")
  (princ (player-pawns *player-1*))
  (princ "\nCurrent Cards: ")
  (princ (player-current-cards *player-1*))

  (princ "Red")
  (princ "Master \n")
  (princ (player-master *player-2*))
  (princ "\nPawns: ")
  (princ (player-pawns *player-2*))
  (princ "\nCurrent Cards: ")
  (princ (player-current-cards *player-2*))
)

;;Legal moves

;;(defun move-generation (player)
;;  
;;)

;;(defun legal-moves (player)
;;  (reduce (lambda (acc item) (append *mantis* acc)) (player-pawns player) :initial-value nil)
;;)

;;Legal moves for individual piece
(defun piece-legal-moves (piece player)
  (reduce 
   (lambda (moves card-rule) 
     (setf new-move (cons (+ (car piece) (car card-rule)) (+ (cdr piece) (cdr card-rule))))
     (if (and (check-boundaries new-move) (not (member new-move (player-pawns player))))
         (cons new-move moves))
     ) (player-active-card player) :initial-value nil)
)

(defun legal-moves (player)
  (reduce (lambda (acc item) (append acc (piece-legal-moves item (player-active-card player)))) (append (list (player-master player)) (player-pawns player)) :initial-value nil)
)

;;Move boundaries
(defun check-boundaries (move)
  (and 
   (< (car move) 6) 
   (> (car move) 0) 
   (< (cdr move) 6) 
   (> (cdr move) 0))
)
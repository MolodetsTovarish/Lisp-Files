;;ONITAMA

;;Structures
(defstruct game win-state side-card red-player blue-player move-records active-player)

(defstruct player color pawns master current-cards strategy)

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
    (list (make-player :color  'red
                       :pawns  '((1 . 1) (2 . 1) (4 . 1) (5 . 1))
                       :master '(3 . 1)
                       :current-cards (cons (first *shuffled-cards*) (second *shuffled-cards*))
                       :strategy nil )))

(setf *player-2* 
    (list (make-player :color  'blue
                       :pawns  '((1 . 5) (2 . 5) (4 . 5) (5 . 5))
                       :master '(3 . 5)
                       :current-cards (cons (fourth *shuffled-cards*) (fifth *shuffled-cards*))
                       :strategy nil)))

(setf *game* 
  (list (make-game :win-state  nil
                   :side-card  (third *shuffled-cards*) ;;third
                   :red-player *player-1*
                   :blue-player *player-2*
                   :move-records nil
                   :active-player nil)))

(print (player-color *player-1*))

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
  (print "Game")
  (print "Blue")
  ()

  (print "Red")
  ()
)
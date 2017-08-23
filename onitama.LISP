;;ONITAMA
(setf *print-circle* t)


;;STRUCTURES

;;A game structure consists of a win state, a side card, move records, and who the active player is
(defstruct game win-state side-card move-records active-player)

;;A player structure consists of a color (red and blue), list of pawn coordinates, the coordinates of the master piece, 
;;the cards in the player's hand, the active card selected for a move (i.e. the tiger card will be applied to a piece),
;;and the type of strategy (human player, random, AI)
(defstruct player color pawns master current-cards active-card strategy)

;;A move structure consists of a current position and final position
(defstruct move current-pos final-pos)


;;PARAMETERS

;;
;;These card parameters are the  coordinate shifts corresponding to a different animal-themed card.
;;The coordinates of these cards are applied to a tile to create the possible moves available for that tile.
;;(i.e., if a boar card ((1, 0), (-1, 0), (0, 1)) is applied to coordinate (3, 3), the possible moves for it are (4, 3), (2, 3) and (3, 4).
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
(defparameter *boar* '((1 . 0) (-1 . 0) (0 . 1)))
(defparameter *tiger* '((0 . 2) (0 . -1)))
(defparameter *dragon* '((-1 . -1) (1 . -1) (-2 . 1) (2 . 1)))
(defparameter *crab* '((0 . 1) (-2 . 0) (2 . 0)))
(defparameter *elephant* '((-1 . 1) (-1 . 0) (1 . 0) (1 . 1)))

;;This is a list of cards which
(defparameter *card-list* (list (cons 'horse *horse*) (cons 'ox *ox*) (cons 'crane *crane*) (cons 'mantis *mantis*)
                                (cons 'eel *eel*) (cons 'cobra *cobra*) (cons 'rooster *rooster*) (cons 'goose *goose*) 
                                (cons 'frog *frog*) (cons 'rabbit *rabbit*) (cons 'monkey *monkey*) (cons 'boar *boar*) 
                                (cons 'tiger *tiger*) (cons 'dragon *dragon*) (cons 'crab *crab*) (cons 'elephant *elephant*)))

;;This function checks if a move is legal with the rules of a card.
;;The starting (original) position and final position are passed in and the horizontal and vertical shifts are calculated.
;;If both shifts match the shift of the card, the move fits the card rule.
(defun check-card-rule (original-pos final-pos card)

   ;;horizontal shift is x axis (car) of final - original coordinate
   (setf h-shift (- (car final-pos) (car original-pos)))
   ;;vertical shift is y axis (cdr) of final - original coordinate
   (setf v-shift (- (cdr final-pos) (cdr original-pos)))
   
   ;;checks if the v and h shifts of the original and final positions match the v and h shifts of the card
   (member nil card 
           :test (lambda (item card-rule) (and (eq h-shift (car card-rule)) (eq v-shift (cdr card-rule)))))    
)

;;Sets up the two players, game state, and shuffles the cards; this is the beginning of the game
(defun setup-game ()

;;Shuffles the card list
(setf *shuffled-cards* (card-shuffle *card-list*))

;;Creates a new player struct for player 1; sets its color (red or blue), starting coordinates for the pawns and master, 
;;takes the first and second card of the shuffled deck to put into current-cards (active-card will be set during gameplay), and sets strategy
(setf *player-1* 
    (make-player :color  'red
                 :pawns  '((1 . 1)
                           (2 . 1) (4 . 1) (5 . 1)
                           )
                 :master '(3 . 1)
                 ;;:current-cards (cons (first *shuffled-cards*) (second *shuffled-cards*))
                 :current-cards (list (first *shuffled-cards*) (second *shuffled-cards*))
                 :active-card *tiger*
                 :strategy nil))

;;Creates a new player struct for player 1; sets its color (red or blue), starting coordinates for the pawns and master, 
;;takes the fourth and fifth card of the shuffled deck to put into current-cards (active-card will be set during gameplay), and sets strategy
(setf *player-2* 
      (make-player :color 'blue
                   :pawns  '((1 . 5)
                             (2 . 5) (4 . 5) (5 . 5))
                   :master '(3 . 5)
                   ;;:current-cards (cons (fourth *shuffled-cards*) (fifth *shuffled-cards*))
                   :current-cards (list (fourth *shuffled-cards*) (fifth *shuffled-cards*))
                   :active-card nil
                   :strategy nil))

;;Sets up new game; win-state is nil at the beginning of the game, the third card of the shuffled deck is the side-card, keeps move records, and has a circular list of active players, which will cycle between player 1 and 2
(setf *game* 
      (make-game :win-state  nil
                 :side-card  (third *shuffled-cards*) ;;third
                 :move-records nil
                 :active-player (circular (list *player-1* *player-2*))))

;;prints all the cards and cards of player 1
(print *shuffled-cards*)
(print (player-current-cards *player-1*))

)

;;This function shuffles the cards
(defun card-shuffle (input-list)
  (loop with l = (length input-list)
        for i below l
        do (rotatef (nth i input-list)
                    (nth (random l) input-list))) ;;cards are rotated in the list and put into random places in the list
  input-list)

;;Print-board-state() prints the pawns and cards held by both players
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

;;LEGAL MOVES

;;This function returns all the legal moves available for an individual piece
(defun piece-legal-moves (piece player)
  (reduce 
   (lambda (moves card-rule) 
     (setf new-move (cons (+ (car piece) (car card-rule)) (+ (cdr piece) (cdr card-rule)))) ;; new move made with original position and a card rule
     (if (and (check-boundaries new-move) (not (member new-move (player-pawns player)))) ;;checks if move is within boundaries and not taken by own pawns
         (cons (cons piece new-move) moves) ;;adds new move to move list
         moves ;;otherwise returns current list
         )
     ) (player-active-card player) :initial-value nil)
)

;;This function returns all the legal moves available with a player's current hand
(defun legal-moves (player)
  (reduce (lambda (acc item)
            (append acc (piece-legal-moves item player))) ;;goes through each piece a player has and gets the legal moves available for each piece
          (cons (player-master player) (player-pawns player)) :initial-value nil)
)

;;Move boundaries; this prevents moves being made that go outside a 5x5 grid
(defun check-boundaries (move)
  (and 
   (< (car move) 6) 
   (> (car move) 0) 
   (< (cdr move) 6) 
   (> (cdr move) 0)) ;;no move can be above 5 or below 1
)

;;Circular list testing ground
(setf *print-circle* t)

(defun circular (items)
  (setf (cdr (last items)) items)
  items)

;;Main game loop consisting of setup, play, and checking if there is a winner
(defun game ()
  (setup-game)
  (play)
  (show-result)
)

;;Shows if the game has been won
(defun show-result ()
  
)

;;Changes between player 1 and 2 by setting the active player to the next element in the circular active-player list property in the game struct
(defun switch-player ()
  
  (setf (game-active-player *game*) (cdr (game-active-player *game*)))
)

;;Returns current player
(defun get-active-player ()
  (car (game-active-player *game*))
)

;;Function for playing of game
(defun play ()
  (if 
      (game-is-over)
      nil) ;;ends if game is over
  (block
      (make-move)
      (play)) ;;keep making moves until the game is over
)

;;a function to check if your move is the winning move
;;(Win conditions: kill the other Master or put your Master on the opposite Throne)
(defun game-is-over () 

)

;;Make-move switches the player and sets the strategy from the player so that it can make the move
(defun make-move () 
  (switch-player)
  (setf strategy (player-strategy (get-active-player)))
  (apply-move (funcall strategy))
)

(defun apply-move ()
  
)

;;STRATEGIES

;;In the human strategy, all moves and card selections are controlled by the player
(defun human-strategy ()
  ;;Active player set
  (setf active-player (get-active-player))
  ;; Choose the card and update the active player's active-card property with it
  (setf (player-active-card active-player)
        (cdr (choice-prompt (player-current-cards active-player) "Select a card from your hand: " (lambda (x) (car x))
        ))) 
  ;; Choose the move 	
  (choice-prompt (legal-moves active-player) "Select a move:" (lambda (x) x))

)

;;This strategy will make moves at random
(defun random-strategy ()

)

;;The choice-prompt() function is responsible for the printing out and formatting of the choices (user selection interface)
(defun choice-prompt (choices prompt-string formatting-function)
  (setf i 0)
  ;;printing out choices
  (format t "~S~%" prompt-string)
  ;;loops through each choice
  (loop for x in choices
        ;;increments the choice number
        do (setf i (1+ i))
        ;;each 'i' is attached to the beginning of the string (ex: "1. Choice 1")
        (format t "~D: ~S~%" i (funcall formatting-function x)) ;;any formatting function can be passed as an argument
        )

  ;;choice selection from keyboard input
   (choice-select (read) choices)
  
)

;;The choice-select() function is used in choice-prompt to select a choice from a list by reading key inputs
(defun choice-select (input choices)
  (if (not (numberp input)) ;;if the input wasn't a number, try again until it is
      (choice-select (read) choices)
      ;;prevents numbers not in choice list from being selected
      (if (or (> input (length choices)) (<= input 0))
          (choice-select (read) choices)
        (nth (- input 1) choices))
    
  )
)


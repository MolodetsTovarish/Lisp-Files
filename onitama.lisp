;;ONITAMA
(setf *print-circle* t)

;; Global vars for the game
(defvar *game*)
(defvar *player-1*)
(defvar *player-2*)

;;STRUCTURES

;;A game structure consists of a win state, a side card, move records, and who the active player is
(defstruct game win-state side-card history active-player)

;;A player structure consists of a color (red and blue), list of pawn coordinates, the coordinates of the master piece, 
;;the cards in the player's hand, the active card selected for a move (i.e. the tiger card will be applied to a piece),
;;and the type of strategy (human player, random, AI)
(defstruct player color direction pawns master master-position pieces current-cards active-card strategy)


;;PARAMETERS
(defparameter *player-1-start-position* '((3 . 1) (1 . 1) (2 . 1) (4 . 1) (5 . 1)))
(defparameter *player-2-start-position* '((3 . 5) (1 . 5) (2 . 5) (4 . 5) (5 . 5)))

;;
;;These card parameters are the  coordinate shifts corresponding to a different animal-themed card.
;;The coordinates of these cards are applied to a tile to create the possible moves available for that tile.
;;(i.e., if a boar card ((1, 0), (-1, 0), (0, 1)) is applied to coordinate (3, 3), the possible moves for it are (4, 3), (2, 3) and (3, 4).
;;
(defparameter *horse* '(horse (0 . 1) (0 . -1) (-1 . 0)))
(defparameter *ox* '(ox (0 . 1) (0 . -1) (1 . 0)))
(defparameter *crane* '(crane (0 . 1) (-1 . -1) (1 . -1)))
(defparameter *mantis* '(mantis (-1. 1) (1 . 1) (0 . -1)))
(defparameter *eel* '(eel (-1 . 1) (-1 . -1) (1 . 0)))
(defparameter *cobra* '(cobra (-1 . 0) (1 . 1) (1 . -1)))
(defparameter *rooster* '(rooster (-1 . -1) (-1 . 0) (1 . 0) (1 . 1)))
(defparameter *goose* '(goose (-1 . 1) (-1 . 0) (1 . 0) (1 . -1)))
(defparameter *frog* '(frog (-2 . 0) (-1 . 1) (1 . -1)))
(defparameter *rabbit* '(rabbit (-1 . -1) (1 . 1) (2 . 0)))
(defparameter *monkey* '(monkey (-1 . 1) (1 . 1) (-1 . -1) (1 . -1)))
(defparameter *boar* '(boar (1 . 0) (-1 . 0) (0 . 1)))
(defparameter *tiger* '(tiger (0 . 2) (0 . -1)))
(defparameter *dragon* '(dragon (-1 . -1) (1 . -1) (-2 . 1) (2 . 1)))
(defparameter *crab* '(crab (0 . 1) (-2 . 0) (2 . 0)))
(defparameter *elephant* '(elephant (-1 . 1) (-1 . 0) (1 . 0) (1 . 1)))

;;This is a list of cards which is shuffled
(defparameter *card-list*
   (list
    *horse* *ox* *crane* *mantis* *eel* *cobra* *rooster* *goose* *frog* *rabbit* *monkey* *boar* *tiger* *dragon* *crab* *elephant*)
)
  ;;Sets up the two players, game state, and shuffles the cards; this is the beginning of the game
(defun setup-game (player-1-strategy player-2-strategy)

  ;;Shuffles the card list
  (let (
        (shuffled-cards
          (card-shuffle *card-list*)
          )
        ) 
    ;;Creates a new player struct for player 1; sets its color (red or blue), starting coordinates for the pawns and master, 
    ;;takes the first and second card of the shuffled deck to put into current-cards (active-card will be set during gameplay), and sets strategy
    (setf *player-1*
          (make-player :color  'red
                       :direction 1
                       :pawns  '((1 . 1)
                                 (2 . 1) (4 . 1) (5 . 1))
                       :master '(3 . 1)
                       :master-position '(3 . 1)
                       :pieces '((3 . 1) (1 . 1) (2 . 1) (4 . 1) (5 . 1))
                       :current-cards (list (first shuffled-cards) (second shuffled-cards))
                       :active-card nil
                       :strategy player-1-strategy))

    ;;Creates a new player struct for player 1; sets its color (red or blue), starting coordinates for the pawns and master, 
    ;;takes the fourth and fifth card of the shuffled deck to put into current-cards (active-card will be set during gameplay), and sets strategy
    (setf *player-2* 
          (make-player :color 'blue
                       :direction -1
                       :pawns  '((1 . 5)
                                 (2 . 5) (4 . 5) (5 . 5))
                       :master '(3 . 5)
                       :master-position '(3 . 5)
                       :pieces '((3 . 5) (1 . 5) (2 . 5) (4 . 5) (5 . 5))
                       :current-cards (list (fourth shuffled-cards) (fifth shuffled-cards))
                       :active-card nil
                       :strategy player-2-strategy))

    ;;Sets up new game; win-state is nil at the beginning of the game, 
    ;;the third card of the shuffled deck is the side-card, keeps move records, 
    ;;and has a circular list of active players, which will cycle between player 1 and 2
    (setf *game* 
          (make-game :win-state nil
                     :side-card (third shuffled-cards) ;;third
                     :history nil
                     :active-player (circular (list *player-2* *player-1*)))
          ;;add initial state property here
          )

    ;;prints all the cards and cards of player 1
    ;;(print *shuffled-cards*)
    ;;(print (player-current-cards *player-1*))

    (setf *player-1-starting-cards* (player-current-cards *player-1*))
    (setf *player-2-starting-cards* (player-current-cards *player-2*))
    (setf *side-starting-card* (game-side-card *game*))
    )
  )

;;This function shuffles the cards
(defun card-shuffle (input-list)
  (loop with l = (length input-list)
        for i below l
        do (rotatef (nth i input-list)
                    (nth (random l) input-list))) ;;cards are rotated in the list and put into random places in the list
  input-list)


;;Prints the positions of the pieces on the board to the corresponding positions in the board array; this is a visual representation of the board
(defun print-board (game)
  ;;Initial array used for printing the board
  (let (
       (board (make-array '(5 5) :initial-element "."))
        (active-player  (get-active-player game))
        (passive-player (get-passive-player game))
        )
    (fill-positions active-player board)
    (fill-positions passive-player board)

    (format t "Side Card: ~a~%~%" (game-side-card game))

    (format t "Player '(~a)' cards: ~a~%~%"  (player-color active-player)  (player-current-cards active-player))

    (format t "(TURN) Player '(~a)' cards: ~a~%~%" (player-color passive-player) (player-current-cards passive-player))

    (loop for i from 4 downto 0 do
      (progn
        (loop for j from 0 to 4 do
          (princ (aref board j i))
              )
        (format t "~%")
        )
          )
    )
  )

;;This function fills the positions in the board for a player, allowing you to select the symbols for the master and pawns
(defun fill-positions (player board)
  ;;Sets the master position on the board; the master is the first element in the pawn list, and its coordinates are extracted
  (setf (aref board 
              ;;x coordinate
              (1- (car (car (player-pieces player)))) 
              ;;y coordinate
              (1- (cdr (car (player-pieces player))))) (get-master-symbol player))

  (mapcar (lambda (x) (setf (aref board 
                                  (1- (car x)) (1- (cdr x))) (get-pawn-symbol player) )) (remove nil (cdr (player-pieces player))))
  )

(defun get-master-symbol (player)
  (if (equal 'blue (player-color player))
      "B"
      "R"
      )
  )

(defun get-pawn-symbol (player)
  (if (equal 'blue (player-color player))
      "b"
      "r"
      )
  )


;;LEGAL MOVES

;;This function returns all the legal moves available for an individual piece
(defun piece-legal-moves (piece player card)
  (reduce 
   (lambda (moves card-rule) 
     (let (
           (new-move
             (cons (+ (car piece) (* (car card-rule) (player-direction player)))
			             (+ (cdr piece) (* (cdr card-rule) (player-direction player))))
             ) ;; new move made with original position and a card rule
           )
     (if (and (check-boundaries new-move) 
              (not (member new-move (player-pieces player) ;;master position appended to pawn list
                           :test #'equal))) ;;checks if move is within boundaries and not taken by own pawns
         (cons (cons piece new-move) moves) ;;adds new move to move list
         moves ;;otherwise returns current list
         )
       )
     ) (cdr card) :initial-value nil)
)

;;This function returns all the legal moves available with a player's active card
(defun card-legal-moves (player card)
  (reduce (lambda (acc piece)
            (if piece ;; only do it for "live" pieces
                (append acc (piece-legal-moves piece player card)) ;;goes through each piece a player has and gets the legal moves available for each piece
                acc
                )
            )
          (player-pieces player) :initial-value nil)
  )

;;This function returns the legal moves available with both of a player's current cards
(defun legal-moves-full (player)
  (append (card-legal-moves player (first (player-current-cards player)))
          (card-legal-moves player (second (player-current-cards player)))
  )
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

;;Main game loop consisting of setup, play, and checking if there is a winner.
;;Random players are used, just to be able to specify strategies.
(defun game ()
  (setup-game #'random-strategy #'random-strategy)
  (play)
  (show-result)
  )

;;Shows if the game has been won
(defun show-result ()
  )

;;Changes between player 1 and 2 by setting the active player to the next element in the circular active-player list property in the game struct
(defun switch-player (game)
  (setf (game-active-player game) (cdr (game-active-player game)))
  )

;;Returns current player
(defun get-active-player (game)
  (car (game-active-player game))
  )

;;Returns passive player
(defun get-passive-player (game)
  (car (cdr (game-active-player game)))
  )

;;Function for playing of game
(defun play ()
  (if
   (game-over *game*)
   nil) ;;ends if game is over
  (progn
    (make-move)
    (play)) ;;keep making moves until the game is over
  )

;;a function to check if your move is the winning move
;; Note: 'active-player' is the one who made the last move
;;(Win conditions: kill the other Master or put your Master on the opposite Throne)
(defun game-over (game)
  ;;condition 1: the master of a player to move is killed
  (or (not (car (player-pieces (get-passive-player game))))
      ;;condition 2: the master lands on the opponent's master's starting position
      (or (equal (car (player-pieces (get-active-player game)))
                 (player-master-position (get-passive-player game))))
      )
  )

;; Reset game
(defun reset-game ()

  (setf (game-history *game*) nil)

  ;;Resets positions
  (set-positions *player-1* *player-1-start-position*)
  (set-positions *player-2* *player-2-start-position*)

  ;;Reset original cards
  (setf (player-current-cards *player-1*) *player-1-starting-cards*)
  (setf (player-current-cards *player-2*) *player-2-starting-cards*)
  (setf (game-side-card *game*) *side-starting-card*)
  )

;;This function automatically plays the game from a list of moves
(defun autoplay (moves)
  ;;default player order
  ;;saves history
  (let (
       (saved-history moves)
        )
    ;;clears history ------- Either (setf (game-history *game*) nil) or (setf moves nil)
    (reset-game)
    ;;applies saved history
    (loop for x in saved-history do (switch-player *game*)
                                    (apply-move *game* x)
          )
    )
  )

;;This function replays the game up until the 2nd last move, undoing the game's most recent move
(defun undo ()
  (let (
        (undone-list
          (remove (car (last (game-history *game*))) (game-history *game*))
          )
        )
    (setf (game-history *game*) nil)
    (autoplay undone-list)
    )
  )

;;This function sets the positions of the pieces of a player
(defun set-positions (player positions)
  (setf (player-pieces player) positions)
  )

;;Switches the player and sets the strategy from the player so that it can make the move
(defun make-move ()
  (switch-player *game*)
  (apply-move *game*
              (funcall
               (player-strategy (get-active-player *game*))
               )
              )

  )

;;Applies move by changing position of piece from it's original position to the new position
;; 
(defun apply-move (game move)
 (let
      (
       (piece-old-pos (car (cdr move)))
       (piece-new-pos (cdr (cdr move)))
       (active-player (get-active-player game))
       (passive-player (get-passive-player game))
       )
  ;; Move the piece to the new position
  (setf (player-pieces (get-active-player game))
        (substitute piece-new-pos
                    piece-old-pos
                    (player-pieces active-player)
                    :test #'equal))
  ;; Eliminate the opponent's piece if it happened to be at the new position
  (setf (player-pieces (get-passive-player game))
        (substitute nil piece-new-pos
                    (player-pieces passive-player)
                    :test #'equal))
  (setf (game-history game) (append (game-history game) (list move) ;;(list (car move) (cdr move))
                                    ))

  game
  )
 )

;;This function swaps the player's selected card with the side card; i.e. if the player selects the goose card, it swaps places with the dragon side card
(defun swap-cards (card player)
  ;;Replace selected card with side card; side card now in player's hand
  (setf (player-current-cards player)
        (substitute (game-side-card *game*) card (player-current-cards player) 
                    :test (lambda (new-pos old-pos) (equal new-pos old-pos))))
  ;;Replace side card with selected card; selected card now side card
  (setf (game-side-card *game*) card)
  )

;;STRATEGIES

;;In the human strategy, all moves and card selections are controlled by the player
(defun human-strategy ()
  ;;Active player set
  (let (
        (active-player (get-active-player *game*))
        )
    (cons

     (car
      ;; Swap selected card with side card
      (swap-cards
       ;; Choose the card and update the active player's active-card property with it
       (setf (player-active-card active-player)
             (choice-prompt (player-current-cards active-player) "Select a card from your hand: " (lambda (x) (car x))
                            ))
       active-player
       )
      )

  ;; Choose the move 	
  (choice-prompt (card-legal-moves active-player (player-active-card active-player)) "Select a move:" (lambda (x) x))

  )
 )
)
;;This strategy will make moves at random
(defun random-strategy ()
  ;;Active player set
  (setf active-player (get-active-player *game*))

(cons
  (swap-cards
  ;; Choose the card and update the active player's active-card property with it
  (setf (player-active-card active-player)
        (nth (random 2) (player-current-cards active-player))
         )
  active-player
  )

  ;;Choose move
  (nth (random (1- (length (card-legal-moves active-player (player-active-card active-player))))) (card-legal-moves active-player (player-active-card active-player)))
  )
)

;;The choice-prompt() function is responsible for the printing out and formatting of the choices (user selection interface)
(defun choice-prompt (choices prompt-string formatting-function)
  ;;printing out choices
  (format t "~S~%" prompt-string)
  (let (
        (i 0)
        )
    ;;loops through each choice
    (loop for x in choices
          ;;increments the choice number
          do
             ;;each 'i' is attached to the beginning of the string (ex: "1. Choice 1")
            (format t "~D: ~S~%" (incf i) (funcall formatting-function x)) ;;any formatting function can be passed as an argument
          )

    ;;choice selection from keyboard input
    (choice-select (read) choices)
    )
  )

;;The choice-select() function is used in choice-prompt to select a choice from a list by reading key inputs
(defun choice-select (input choices)
  (if (not (numberp input)) ;;if the input wasn't a number, try again until it is
      (choice-select (read) choices)
      ;;prevents numbers not in choice list from being selected
      (if (or (> input (length choices)) (<= input 0))
          (choice-select (read) choices)
          (nth (- input 1) choices)
      )
  )
)

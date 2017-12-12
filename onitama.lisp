;;ONITAMA
(setf *print-circle* t)

;; Global vars for the game
(defvar *game*)
(defvar *player-1*)
(defvar *player-2*)

;;STRUCTURES

;;A game structure consists of a win state, a side card, move records, and who
;;the active player is
(defstruct game win-state side-card history active-player
  side-starting-card player-1-starting-cards player-2-starting-cards)

;;A player structure consists of a color (red and blue), list of pawn coordinates,
;;the coordinates of the master piece,
;;the cards in the player's hand, the active card selected for a move
;;(i.e. the tiger card will be applied to a piece),
;;and the type of strategy (human player, random, AI)
(defstruct player color direction pawns master master-position pieces current-cards  strategy)


;;PARAMETERS

;;
;;These card parameters are the  coordinate shifts corresponding to a different
;;animal-themed card.
;;The coordinates of these cards are applied to a tile to create the possible
;;moves available for that tile.
;;(i.e., if a boar card ((1, 0), (-1, 0), (0, 1)) is applied to
;;coordinate (3, 3), the possible moves for it are (4, 3), (2, 3) and (3, 4).
;;
(defparameter *horse*
  '(horse (:up 1)
          (:down 1)
          (:left 1)))
(defparameter *ox*
  '(ox (:up 1)
       (:down 1)
       (:right 1)))
(defparameter *crane*
  '(crane (:up 1)
          (:left 1 :down 1)
          (:right 1 :down 1)))
(defparameter *mantis*
  '(mantis (:left 1 :up 1)
           (:right 1 :up 1)
           (:down 1)))
(defparameter *eel*
  '(eel (:left 1 :up 1)
        (:left 1 :down 1)
        (:right 1)))
(defparameter *cobra*
  '(cobra (:left 1)
          (:right 1 :up 1)
          (:right 1 :down 1)))
(defparameter *rooster*
  '(rooster (:left 1 :down 1)
            (:left 1)
            (:right 1)
            (:right 1 :up 1)))
(defparameter *goose*
  '(goose (:left 1 :up 1)
          (:left 1)
          (:right 1)
          (:right 1 :down 1)))
(defparameter *frog*
  '(frog (:left 2)
         (:left 1 :up 1)
         (:right 1 :down 1)))
(defparameter *rabbit*
  '(rabbit (:left 1 :down 1)
           (:right 1 :up 1)
           (:right 2)))
(defparameter *monkey*
  '(monkey (:left 1 :up 1)
           (:right 1 :up 1)
           (:left 1 :down 1)
           (:right 1 :down 1)))
(defparameter *boar*
  '(boar (:right 1)
         (:left 1)
         (:up 1)))
(defparameter *tiger*
  '(tiger (:up 2)
          (:down 1)))
(defparameter *dragon*
  '(dragon (:left 1 :down 1)
           (:right 1 :down 1)
           (:left 2 :up 1)
           (:right 2 :up 1)))
(defparameter *crab*
  '(crab (:up 1)
         (:left 2)
         (:right 2)))
(defparameter *elephant*
  '(elephant (:left 1 :up 1)
             (:left 1)
             (:right 1)
             (:right 1 :up 1)))

;; List of cards, built from their descriptions.
(defun card-list ()
  (mapcar #'card (list *horse* *ox* *crane* *mantis* *eel* *cobra*
                       *rooster* *goose* *frog* *rabbit* *monkey*
                       *boar* *tiger* *dragon* *crab* *elephant*))
)

;;Sets up the two players, game state, and shuffles the cards;
;;this is the beginning of the game
(defun setup-game (player-1-strategy player-2-strategy)
  
;;This function shuffles the cards
(labels ((card-shuffle (input-list)
  (loop with l = (length input-list)
        for i below l
        do (rotatef (nth i input-list)
                    (nth (random l) input-list)))
  ;;cards are rotated in the list and put into random places in the list
  input-list))

(let* (
      ;;Shuffle the card list
       (shuffled-cards
        (card-shuffle (card-list)
          )
        )
       ;;)
    ;;Creates a new player struct for player 1; sets its color (red or blue),
    ;;starting coordinates for the pawns and master,
    ;;takes the first and second card of the shuffled deck to put into
    ;;current-cards, and sets strategy

    ;(setf 
     (*player-1*
          (create-player  'red
                        (list (first shuffled-cards) (second shuffled-cards))
                        player-1-strategy) )
         ; )

    ;;Creates a new player struct for player 1; sets its color (red or blue),
    ;;starting coordinates for the pawns and master,
    ;;takes the fourth and fifth card of the shuffled deck to put into
    ;;current-cards, and sets strategy

    ;;(setf
     (*player-2*
          (create-player 'blue
                        (list (fourth shuffled-cards) (fifth shuffled-cards))
                        player-2-strategy) )
          ;;)

    )

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


    (setf (game-player-1-starting-cards *game*) (player-current-cards *player-1*))
    (setf (game-player-2-starting-cards *game*) (player-current-cards *player-2*))
    (setf (game-side-starting-card *game*) (game-side-card *game*))
    ))
  )

(defun create-player (color current-cards strategy)
  (labels
      (
       (create-player1 (color direction pawns master
                        current-cards strategy)
                      (make-player :color color
                                   :direction direction
                                   :pawns pawns
                                   :master master
                                   :master-position master
                                   :pieces (cons master pawns)
                                   :current-cards current-cards
                                   :strategy strategy
                        )

        )
       )
  (if (equal color 'red)
      (create-player1 'red 1  '((1 . 1) (2 . 1) (4 . 1) (5 . 1))
                      '(3 . 1) current-cards strategy)
      (create-player1 'blue -1 '((1 . 5) (2 . 5) (4 . 5) (5 . 5))
                      '(3 . 5) current-cards strategy)

      )
    )
  )



;;Prints the positions of the pieces on the board to the corresponding positions
;;in the board array; this is a visual representation of the board
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

    (format t "Player '(~a)' cards: ~a~%~%"
            (player-color active-player)  (player-current-cards active-player))

    (format t "(TURN) Player '(~a)' cards: ~a~%~%"
            (player-color passive-player) (player-current-cards passive-player))

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

;;This function fills the positions in the board for a player,
;;allowing you to select the symbols for the master and pawns
(defun fill-positions (player board)
  ;;Sets the master position on the board; the master is the first element
  ;;in the pawn list, and its coordinates are extracted
  (setf (aref board
              ;;x coordinate
              (1- (car (car (player-pieces player))))
              ;;y coordinate
              (1- (cdr (car (player-pieces player))))) (get-master-symbol player))

  (mapcar (lambda (x) (setf (aref board
                                  (1- (car x)) (1- (cdr x)))
                            (get-pawn-symbol player) ))
          (remove nil (cdr (player-pieces player))))
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
(defun legal-moves (player)
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

;;Changes between player 1 and 2 by setting the active player
;;to the next element in the circular active-player list property in the game struct
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

  (let ((*player-1-start-position* '((3 . 1) (1 . 1) (2 . 1) (4 . 1) (5 . 1)))
        (*player-2-start-position* '((3 . 5) (1 . 5) (2 . 5) (4 . 5) (5 . 5)))
        )

  (setf (game-active-player *game*) (circular (list *player-2* *player-1*)))

  (setf (game-history *game*) nil)

  ;;Resets positions
  (set-positions (cdr (game-active-player *game*)) *player-1-start-position*)
  (set-positions (car (game-active-player *game*))  *player-2-start-position*)

  ;;Reset original cards
  (setf (player-current-cards (cdr (game-active-player *game*))) (game-player-1-starting-cards *game*))
  (setf (player-current-cards (car (game-active-player *game*))) (game-player-2-starting-cards *game*))
  (setf (game-side-card *game*) (game-side-starting-card *game*))
  )
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

;;This function replays the game up until the 2nd last move,
;;undoing the game's most recent move
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

;;Applies move by changing position of piece from it's original position
;;to the new position
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
    (setf (game-history game) (append (game-history game) (list move)
                                      ))

    ;;Swap cards
    (swap-cards (car move) active-player)

    game
    )
  )

;; This function swaps the player's selected card with the side card;
;; i.e. if the player selects the goose card, it swaps places with the dragon side card
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
  (let* (
        (active-player (get-active-player *game*))
        (chosen-card
          (choice-prompt (player-current-cards active-player)
                         "Select a card from your hand: " (lambda (x) (car x))
                         )

          )
        )
    (cons

     chosen-card

     ;; Choose the move
     (choice-prompt (card-legal-moves active-player chosen-card)
                    "Select a move:" (lambda (x) x))

     )
    )
  )

;;This strategy will make moves at random
(defun random-strategy ()
  ;;Active player set
  (let*
      (
       (active-player (get-active-player *game*))
       (chosen-card (nth (random 2) (player-current-cards active-player)))
       (moves (card-legal-moves active-player chosen-card))
       )
    (cons

     chosen-card

       ;;Choose move
       (nth (random (1- (length moves))) moves)
       ;;)
     )
    )
)

;;The choice-prompt() function is responsible for the printing out and
;;formatting of the choices (user selection interface)
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
             (format t "~D: ~S~%" (incf i) (funcall formatting-function x))
             ;;any formatting function can be passed as an argument
          )

    ;;choice selection from keyboard input
    (choice-select (read) choices)
    )
  )

;;The choice-select() function is used in choice-prompt to select a choice from
;;a list by reading key inputs
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

;; Turn verbose card rule into the internal representation.
;; Example:
;; (card-rule :up 1 :left 2) -> (1 . -1)
;; i.e. the current internal implementation is a cons cell
;; (but this could be changed/swapped in the future).
;; TODO: in presence of both 'up', 'down'
;; or 'right', 'left', the 2nd value is ignored.
;; Consider throwing an error instead.
(defun card-rule (&key up down right left)
  (labels (
           (shift-sign (dir1 dir2)
             ;; if 1st param is present (i.e. not nil), return it's
             ;; positive value; otherwise if 2nd is present, return the negative value.
             ;; If both are nil, return 0.
             (or
             (cond
                    (
                     dir1 dir1
                    )
                    (
                     dir2 (- dir2)
                    )
             ) 0)
             ))
    (cons
     (shift-sign right left)
     (shift-sign up down)
     )

    )
  )

;; Card constructor.
;; Construct the card from the card description.
;; Current implementation assumes the first element of
;; the card description is a card name followed by the card rules.
(defun card (card-description)
  (cons (car card-description)
        (mapcar (lambda (rule)
                  (apply #'card-rule rule)
                  )
                  (cdr card-description))
                )
  )

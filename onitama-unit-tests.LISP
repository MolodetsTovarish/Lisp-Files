(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-game-over ()
 (and 
 (progn
   (setup-game #'human-strategy #'random-strategy) 
   (not (game-over *game*))
   )

 (progn 
   (set-positions *player-2* '((3 . 1) (1 . 1) (2 . 1) (4 . 1) (5 . 1)))
   (game-over *game*)
 )

(progn 
  (make-move)
   (set-positions *player-1* '((3 . 5) (1 . 1) (2 . 1) (4 . 1) (5 . 1)))
   (game-over *game*)
 )

 )
 
)

(defun test-legal-moves ()
  (progn 
    (setup-game #'human-strategy #'random-strategy)
    (make-move)

    (setf (player-current-cards *player-1*) '((ELEPHANT ((-1 . 1) (-1 . 0) (1 . 0) (1 . 1))) 
                                              (RABBIT ((-1 . -1) (1 . 1) (2 . 0))))
          )

    

    (equal (legal-moves *player-1*) 
           ;;'(((3 . 1) 5 . 1) ((3 . 1) 4 . 2) ((1 . 1) 3 . 1) ((1 . 1) 2 . 2) ((2 . 1) 3 . 2) ((4 . 1) 5 . 2))
    )

)
)

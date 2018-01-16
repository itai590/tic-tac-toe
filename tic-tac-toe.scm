;Basic Function
(define board '(- - - - - - - - -))

(define (Print_Board board)
  (cond
    ((empty? board)(newline))
    ((= (remainder (length board) 3) 1)(Graphic_Print (first board))(newline)(Print_Board (rest board)))
    (else (Graphic_Print (first board))(Print_Board (rest board)))))


(define (Update list sign place)
  (cond
    ((empty? list) '())
    ((= place 1) (cons sign (rest list)))
    (else (cons (first list) (Update (rest list) sign (sub1 place))))))
  
(define (Get board place)
  (list-ref board (sub1 place)))
  
(define Winners '((1 2 3)(4 5 6)(7 8 9)(1 4 7)(2 5 8)(3 6 9)(1 5 9)(3 5 7)))
(define Winners2 '((1 2 3)(4 5 6)(7 8 9)(1 4 7)(2 5 8)(3 6 9)(1 5 9)(3 5 7)))
  
(define (Trio? shlasha sign board)
  (and
   (and (equal? sign (Get board (first shlasha)))
        (equal? sign (Get board (second shlasha)))
        (equal? sign (Get board (third shlasha))))))
  
(define (Win? board Winners sign)
  (cond
    ((empty? Winners) #f)
    ((Trio? (first Winners) sign board) #t)
    (else (Win? board (rest Winners) sign))))
  
(define (Free? board place)
  (equal? (Get board place) '-))
  
(define (Full? b)
  (cond
    ((empty? b) #t)
    ((equal? (first b) '-) #f)
    (else (Full? (rest b)))))
  
  
(define (Legal? board place)
  (and (and (and (number? place)(> place 0))(< place 10))(Free? board place)))

(define (Player_Input board sigh read)
  (cond
    ((Legal? board read) (Computer (Update board 'X read)))
    (else (newline) (display "invalid input, please enter new number from [1] to [9]") (newline)(Player board))))
;------------------------------------------------------------------------------------------------------------------

;The Hard Level
(define (Player board)
  (newline)
  (display "It's your Move, please enter your loaction (you are the X)")(newline) (display"[1] [2] [3]") (newline) (display"[4] [5] [6]")(newline) (display"[7] [8] [9]")(newline)
  (cond
    ((Win? board Winners 'O) (Print_Board board) (display "You Lost! hit [1] to play again or [2] to exit"))
    ((Full? board) (Print_Board board) (display "Tie! hit [1] to play again or [2] to exit"))
    (else (newline) (Print_Board board) (display "play") (Player_Input board 'X (read)))))


(define (Attack_Strategy board Winners)
  (cond
    [(empty? Winners) (Defence_Strategy board Winners2)]
    
    [(and (and (equal? (Get board (first (first Winners))) 'O) (equal? (Get board (second (first Winners))) 'O)) (Free? board (third (first Winners)))) (third (first Winners))]
    
    [(and (and (equal? (Get board (first (first Winners))) 'O) (equal? (Get board (third (first Winners))) 'O)) (Free? board (second (first Winners)))) (second (first Winners))]
    
    [(and (and (equal? (Get board (second (first Winners))) 'O) (equal? (Get board (third (first Winners))) 'O)) (Free? board (first (first Winners)))) (first (first Winners))]
    
    (else (Attack_Strategy board (rest Winners)))))


(define (Defence_Strategy board Winners2)
  (cond
    [(empty? Winners2) '5]
    
    [(and (and (equal? (Get board (first (first Winners2))) 'X) (equal? (Get board (second (first Winners2))) 'X)) (Free? board (third (first Winners2)))) (third (first Winners2))]
       
    [(and (and (equal? (Get board (first (first Winners2))) 'X) (equal? (Get board (third (first Winners2))) 'X)) (Free? board (second (first Winners2)))) (second (first Winners2))]
    
    [(and (and (equal? (Get board (second (first Winners2))) 'X) (equal? (Get board (third (first Winners2))) 'X)) (Free? board (first (first Winners2)))) (first (first Winners2))]
    
    (else (Defence_Strategy board (rest Winners2)))))

    
(define (Computer_Output board sigh place)
  (cond
    ((Legal? board place) (Player (Update board 'O place)))
    (else (Computer_Output board sigh (add1 (random 9))))))

(define (Computer board)
  (cond
    ((Win? board Winners 'X) (Print_Board board) (display "You Won! hit [1] to play again or [2] to exit") )
    ((Full? board) (Print_Board board) (display "Tie! hit [1] to play again or [2] to exit"))
    ((equal? board '(X - - - O - - - X)) (Player (Update board 'O (list-ref '(2 4 6 8) (random 4)))))
    ((equal? board '(- - X - O - X - -)) (Player (Update board 'O (list-ref '(2 4 6 8) (random 4)))))
    ((equal? board '(- - O - X - X - -)) (Player (Update board 'O (list-ref '(1 9) (random 2)))))
    ((equal? board '(- - X - X - O - -)) (Player (Update board 'O (list-ref '(1 9) (random 2)))))
    ((equal? board '(X - - - X - - - O)) (Player (Update board 'O (list-ref '(3 7) (random 2)))))
    ((equal? board '(O - - - X - - - X)) (Player (Update board 'O (list-ref '(3 7) (random 2)))))
    ((equal? board '(- - - - X - - - -)) (Player (Update board 'O (list-ref '(1 3 7 9) (random 4)))))
    ((equal? board '(- - - - O - - - -)) (Player (Update board 'O (list-ref '(1 3 7 9) (random 4)))))
    (else (Computer_Output board 'O (Attack_Strategy board Winners)))))
;------------------------------------------------------------------------------------------------------------------

;The Grafic Show
(define (Graphic_Print shape)
  (cond
   ((equal? shape 'X) (display " X "))
   ((equal? shape 'O) (display " O "))
   ((equal? shape '-) (display " - "))))
;------------------------------------------------------------------------------------------------------------------


;Run The game
(define (Start)
  
    (display "Choose your level") (newline) (display "1-Eazy") (newline)(display "2-Medium") (newline)(display "3-Hard") (newline)(Level_Chooser (read))
   (cond 
    ((equal? (read) 1) (newline) (display "** New game started **") (newline) (Start))
    (else (display "Bye Bye :("))))


(define (Level_Chooser read)
  (cond 
    ((equal? read '1) (Player1 board))
    ((equal? read '2) (Player2 board))
    (else (Player board))))
;------------------------------------------------------------------------------------------------------------------


;The Easy Lavel
(define (Player1 board)
  (newline)
 (display "It's your Move, please enter your loaction (you are the X)")(newline) (display"[1] [2] [3]") (newline) (display"[4] [5] [6]")(newline) (display"[7] [8] [9]")(newline)
  (cond
    ((Win? board Winners 'O) (Print_Board board) (display "You Lost! hit [1] to play again or [2] to exit"))
    ((Full? board)  (Print_Board board) (display "Tie! hit [1] to play again or [2] to exit"))
    (else (newline) (Print_Board board) (display "play") (Player_Input1 board 'X (read)))))

 
(define (Player_Input1 board sigh read)
  (cond
    ((Legal? board read) (Computer1 (Update board 'X read)))
    (else (newline) (display "invalid input, please enter new number from [1] to [9]") (Player1 board))))

(define (Computer1 board)
  (cond
    ((Win? board Winners 'X) (Print_Board board) (display "You Won! hit [1] to play again or [2] to exit"))
    ((Full? board) (Print_Board board) (display "Tie! hit [1] to play again or [2] to exit")) 
    (else  (Computer_Output1 board 'O (add1 (random 9))))))

(define (Computer_Output1 board sigh place)
  (cond
    ((Legal? board place) (Player1 (Update board 'O place)))
    (else (Computer_Output1 board sigh (add1 (random 9))))))
;------------------------------------------------------------------------------------------------------------------


;The Medium Lavel
(define (Player2 board)
  (newline)
 (display "It's your Move, please enter your loaction (you are the X)")(newline) (display"[1] [2] [3]") (newline) (display"[4] [5] [6]")(newline) (display"[7] [8] [9]")(newline)
  (cond
    ((Win? board Winners 'O) (Print_Board board) (newline) (display "You Lost! hit [1] to play again or [2] to exit"))
    ((Full? board) (Print_Board board) (newline) (display "Tie! hit [1] to play again or [2] to exit"))
    (else (newline) (Print_Board board) (display "play") (Player_Input2 board 'X (read)))))


(define (Player_Input2 board sigh read)
  (cond
    ((Legal? board read) (Computer2 (Update board 'X read)))
    (else (newline) (display "invalid input, please enter new number from [1] to [9]") (Player2 board))))

(define (Computer2 board)
  (cond
    ((Win? board Winners2 'X) (Print_Board board) (display "You Won! hit [1] to play again or [2] to exit"))
    ((Full? board) (Print_Board board) (display "Tie! hit [1] to play again or [2] to exit"))
    (else  (Computer_Output2 board 'O (Defence_Strategy2 board Winners2)))))

(define (Computer_Output2 board sigh place)
  (cond
    ((Legal? board place) (Player2 (Update board 'O place)))
    (else  (Computer_Output2 board 'O (add1(random 9))))))


(define (Defence_Strategy2 board Winners2)
  (cond
  [(empty? Winners2) (add1 (random 9))]

   [(and (and (equal? (Get board (first (first Winners2))) 'X) (equal? (Get board (second (first Winners2))) 'X)) (Free? board (third (first Winners2)))) (third (first Winners2))]
       
  [(and (and (equal? (Get board (first (first Winners2))) 'X) (equal? (Get board (third (first Winners2))) 'X)) (Free? board (second (first Winners2)))) (second (first Winners2))]
    
  [(and (and (equal? (Get board (second (first Winners2))) 'X) (equal? (Get board (third (first Winners2))) 'X)) (Free? board (first (first Winners2)))) (first (first Winners2))]
    
    (else (Defence_Strategy2 board (rest Winners2)))))
;------------------------------------------------------------------------------------------------------------------

(display "Welcome to tic-tac-toe:")(newline)

;Start The Game
(Start)
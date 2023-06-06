#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
          next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

;;(define (countX board)
  ;;  board is
    ;;(if (car board 'X)( + 1 count))
; Check whether a list is a valid board
(define (board? lst)  
  (define (board-helper lst xs os)
    (if (empty? lst)
        (if (equal? xs os) #t (if (equal? xs (+ os 1)) #t #f))
    (match (car lst)
      ['X (board-helper (cdr lst) (+ xs 1) os)]
      ['O (board-helper (cdr lst) xs (+ os 1))]
      ['E (board-helper (cdr lst) xs os)]
      [_ #f]
    )
    )
    )
  (cond
    [(integer? (sqrt (length lst))) (board-helper lst 0 0)]
    [else #f]
    )
  )

;; From the board, calculate who is making a move this turn
(define (next-player board)
  (define (count board xs os)
    (if (empty? board) ;;if the board is empty, you have gone through each element, otherwise continuing counting
        (if (> xs os) 'O 'X) ;; if there is more x's than o's, it is O's turn, otherwise it is X's turn
    (match (car board)
      ['X (count (cdr board) (+ xs 1) os)]
      ['O (count (cdr board) xs (+ os 1))]
      [_ (count (cdr board) xs os)]
     )
    )
    )
  (count board 0 0) ;;Computes how many X's and O's there are  
  )

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  (define (finder i board)
    (if (equal? i 0)
        (if (equal? (car board) 'E) #t #f)
        (finder (- i 1) (cdr board))
        )
    )
  (let
      (
      [k (sqrt (length board))])
  (if (and (number? col) (and (number? row) (and (list? board) ;;This if checks to see if all the values given are valid first
                               (or (equal? player 'X) (equal? player 'O)))))
    (if (equal? player (next-player board)) ;;This makes sure it is the player whose attempting turn
        (if (and (> k row) (> k col)) ;; This is if ensures that row and column are not too big to be handled by the program
            (if (and (>= row 0) (>= col 0)) ;; ensures row and col are non-negative
                (finder (+ (* row k) col) board)
             #f
             )
        #f
        )
        #f
    )
    #f
    )
    )
  )

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (define (finder i board len)
     (cond
       [(equal? i 0) (cons player (finder (- i 1) (cdr board) (- len 1)))]
       [(> 0 i) (if (equal? len 0) '()
                    (cons (car board) (finder i (cdr board) (- len 1))))]
       [else (cons (car board) (finder (- i 1) (cdr board) (- len 1)))]
        )
    )
  (let
      ([k (sqrt (length board))]
       [len (length board)])
    (finder (+ (* row k) col) board len)
    )
  )

;;; To determine whether there is a winner?
(define (winner? board)
  (define len (sqrt (length board)))
  
  (define (row-jump brd i)
    (cond
      [(empty? brd) '()]
      [(equal? i 0) brd]
      [else (row-jump (cdr brd) (- i 1))]
    )
   )

  (define (full? brd player matches i)
    (cond
      [(equal? matches len) #t]
      [(empty? (cdr brd)) (equal? (car brd) player)]
      [(equal? (car brd) player) (full? (row-jump brd i) player (+ matches 1) i)]
      [else #f]
     )
    )

  (define (col-check brd cs)
    (cond
      [(equal? cs 0) #f]
      [(equal? (car brd) 'X)
       (cond
         [(full? (row-jump brd len) 'X 1 len) 'X]
         [else (col-check (cdr brd) (- cs 1))]
         )]
      [(equal? (car brd) 'O)
       (cond
         [(full? (row-jump brd len) 'O 1 len) 'O]
         [else (col-check (cdr brd) (- cs 1))]
         )]
      [else (col-check (cdr brd) (- cs 1))]
      )
    )

  (define (row-check brd rs)
    (cond
      [(equal? rs 0) #f]
      [(equal? (car brd) 'X)
       (cond
         [(full? (cdr brd) 'X 1 1) 'X]
         [else (row-check (row-jump brd len) (- rs 1))]
         )]
      [(equal? (car brd) 'O)
       (cond
         [(full? (cdr brd) 'O 1 1) 'O]
         [else (row-check (row-jump brd len) (- rs 1))]
         )]
      [else (row-check (row-jump brd len) (- rs 1))]
      )
    )

  (define (dia-check brd)
    (define diahelper
       (cond
            [(equal? (car (row-jump brd (- len 1))) 'X)
             (cond
               [(full? (row-jump brd (* 2(- len 1))) 'X 1 (- len 1)) 'X]
               )]
            [(equal? (car (row-jump brd (- len 1))) 'O)
             (cond
               [(full? (row-jump brd (* 2(- len 1))) 'O 1 (- len 1)) 'O]
               )]
            [else #f]
            )
      )
       
    (cond
      [(equal? (car brd) 'X)
       (cond
         [(full? (row-jump brd (+ len 1)) 'X 1 (+ len 1)) 'X]
         [else diahelper]
         )]
      [(equal? (car brd) 'O)
       (cond
         [(full? (row-jump brd (+ len 1)) 'O 1 (+ len 1)) 'O]
         [else diahelper]
         )]
      [else diahelper]
      )
   )
  (cond
    [(equal? (col-check board len) 'O) 'O]
    [(equal? (col-check board len) 'X) 'X]
    [(equal? (row-check board len) 'O) 'O]
    [(equal? (row-check board len) 'X) 'X]
    [(equal? (dia-check board) 'O) 'O]
    [(equal? (dia-check board) 'X) 'X]
    [else #f]
    )
  
  )

(define (calculate-next-move x y) x)


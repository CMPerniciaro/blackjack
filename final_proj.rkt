;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)
(require 2htdp/image)
(require 2htdp/planetcute)

;; Data Definitions
         
;; A Game-State is :
;; - (make-GS State LOC Number)
(define-struct GS (state shoe wallet))

;; define template for GS
#;
(define (game-state GS ...)
  ... (GS-state GS) ...           ;the state of the same
  ... (GS-shoe GS)  ...           ;the list of cards that make up the deck
  ... (GS-wallet)   ...)          ;the player's money count (number)


(define-struct Base-State (completed-split current-hand pending-split bet split-bet))
(define-struct Double-State (current-hand bet))
(define-struct Split-State (current-hand bet))
(define-struct Split-Double-State (current-hand bet))
(define-struct Round-Over-State (current-hand))
(define-struct Game-Over-State (current-hand))
#|
A State is:
- (make-Base-State (LOC or #f) LOC (LOC or #f) Number (Number or #f)  (PC1)

   INV: current-hand, pending-split cannot be bust;
        cannot have (LOC) LOC (LOC)
        cannot have #f LOC #f AND Number Number
        cannot have #f LOC LOC AND Number #f
        if pending-split is LOC <=> then bet 2 is Number 


- (make-Double-State LOC Number)                           (PC2)
- (make-Split-State LOC Number)                            (PC3)
- (make-Split-Double-State LOC Number)                     (PC4)
- (make-Round-Over-State LOC)
- (make-Game-Over-State LOC)
|#

#;
(define (state-template s ...)
  (cond
    [(Base-State? s)  ... (Base-State-completed-split s) ...   
                      ... (Base-State-current-hand s) ...      
                      ... (Base-State-pending-split s) ...     
                      ... (Base-State-bet s)    ...               
                      ... (Base-State-split-bet s) ...]
    [(Double-State? s) ... (Double-State-current-hand s) ...  
                       ... (Double-State-bet s) ...]
    [(Split-State? s) ... (Split-State-current-hand s) ...  
                      ... (Split-State-bet s) ...]
    [(Split-Double-State? s)  ... (Split-Double-State-current-hand s) ...  
                              ... (Split-Double-State-bet s) ...]
    [(Round-Over-State? s) ... (Round-Over-State-current-hand s) ...]
    [(Game-Over-State? s)  ... (Game-Over-State-current-hand s) ...]))

; A Deck is:
; - [List of Card]
; - '()
  
; A Wallet is:
; - Positive Number
; - '()

; A Suit is either:
; - "Diamond"
; - "Heart"
; - "Club"
; - "Spade"

; A Value is either:
; - 2 - 10
; - "Jack"
; - "Queen"
; - "King"
; - "Ace"

; A Card is:
; - (make-card Value Suit)
(define-struct card (value suit))

; A LOC is:
; - [List-of Card]

;;[List-of Card]
;; a [Listof X] is either:
;; - '()
;; - (cons X [Listof X])

(define LOC1 (cons (make-card 10 "Spade") (cons (make-card 11 "Diamond")'())))
(define LOC2 (list 1 2 3 4))
(define deck1 (list
               (make-card 2 "Club")
               (make-card 3 "Club")
               (make-card 4 "Club")
               (make-card 5 "Club")
               (make-card 6 "Club")
               (make-card 7 "Club")))

;; card images
(define two-diamond
  (overlay
   (text "2" 45 "red")
   (overlay/align "left" "top"
                  (rhombus 40 45 "solid" "red")
                  (overlay/align "right" "bottom"
                                 (rhombus 40 45 "solid" "red")
                                 (rectangle 200 300 'outline' "blue")))))

(define club
  (overlay/offset
   (overlay/offset
    (circle 15 "solid" "black")
    0
    21
    (overlay/offset
     (circle 15 "solid" "black")
     26 0
     (circle 15 "solid" "black")))
   0
   25
   (triangle 25 "solid" "black")))

(define two-club (overlay
                  (text "2" 45 "black")
                  (overlay/align "left" "top"
                                 club
                                 (overlay/align "right" "bottom"
                                                club
                                                (rectangle 200 300 'outline' "blue")))))

(define heart-img
  (overlay/offset
   (rotate 22
           (rhombus 40 45 "solid" "red"))
   26
   0
   (rotate 160
           (rhombus 40 45 "solid" "red"))))

(define two-heart (overlay
                   (text "2" 45 "red")
                   (overlay/align "left" "top"
                                  heart-img
                                  (overlay/align "right" "bottom"
                                                 heart-img
                                                 (rectangle 200 300 'outline' "blue")))))


(define spade-img
  (overlay/offset 
   (flip-vertical
    (overlay/offset
     (rotate 22
             (rhombus 40 45 "solid" "black"))
     26
     0
     (rotate 160
             (rhombus 40 45 "solid" "black"))))
   0
   20
   (triangle 25 "solid" "black")))

(define two-spade (overlay
                   (text "2" 45 "black")
                   (overlay/align "left" "top"
                                  spade-img
                                  (overlay/align "right" "bottom"
                                                 spade-img
                                                 (rectangle 200 300 'outline' "blue")))))


  
                                                 


;; draw : LOC -> Image
;; takes in a list, and draws the correct card 
(define (draw LOC)
  (cond
    [(and (equal? (card-value (first LOC)) 2) (equal? (card-suit (first LOC)) "Diamond"))
     two-diamond]
    [(and (equal? (card-value (first LOC)) 2) (equal? (card-suit (first LOC)) "Club"))
     two-club]
    [(and (equal? (card-value (first LOC)) 2) (equal? (card-suit (first LOC)) "Spade"))
     two-spade]
    [(and (equal? (card-value (first LOC)) 2) (equal? (card-suit (first LOC)) "Heart"))
     two-heart]
    [else #f]))

(check-expect (draw (list (make-card 2 "Diamond")
                          (make-card 4 "Diamond")))
              two-diamond)
(check-expect (draw (list (make-card 2 "Club")
                          (make-card 4 "Diamond")))
              two-club)



;; add-list : LOC -> Number
;; adds up the elements of a LOC
(define (add-list LOC)
  (cond
    [(empty? LOC) 0]
    [else       (+ (card-value (first LOC))
                   (add-list (rest LOC)))]))

(check-expect (add-list '()) 0)
(check-expect (add-list (list
                         (make-card 2 "Heart")
                         (make-card 2 "Diamond")))
              4)

(check-expect (add-list (list
                         (make-card 7 "Club")
                         (make-card 5 "Heart")
                         (make-card 2 "Diamond")))
              14)


;(define (state-transition s shoe wallet key)
;  (cond
;    [(Base-State? s)
;     (cond
;       [(string=? "h")
;                      ... (Base-State-completed-split s) ...   ;f
;                      ... (Base-State-current-hand s) ...      
;                      ... (Base-State-pending-split s) ...     
;                      ... (Base-State-bet s)    ...               
;                      ... (Base-State-split-bet s) ...]
;       [(string=? " ")
;                      ... (Base-State-completed-split s) ...   
;                      ... (Base-State-current-hand s) ...      
;                      ... (Base-State-pending-split s) ...     
;                      ... (Base-State-bet s)    ...               
;                      ... (Base-State-split-bet s) ...])
;    [(Double-State? s) ;... (Double-State-current-hand s) ...  
;                       ;... (Double-State-bet s) ...
;                       #f]
;    [(Split-State? s) ;... (Split-State-current-hand s) ...  
;                      ;... (Split-State-bet s) ...
;     #f]
;    [(Split-Double-State? s)  ;... (Split-Double-State-current-hand s) ...  
;                             ; ... (Split-Double-State-bet s) ...
;     #f]
;    [(Round-Over-State? s) ;... (Round-Over-State-current-hand s) ...
;     #f]
;    [(Game-Over-State? s)  ;... (Game-Over-State-current-hand s) ...
;     #f]))
              

;; gs-transition : GS key -> GS
;; takes in a GS and key, returns new GS depending on key
(define (gs-transition GS key)
  (state-transition (GS-state GS)            
                    (GS-shoe GS)             
                    (GS-wallet)
                    key))

;
;(check-expect (gs-transition (make-GS (make-Base-State #f
;                                                       (list (make-card 7 "Diamond")
;                                                             (make-card 5 "Club"))
;                                                       #f
;                                                       1
;                                                       #f)
;                                      (list
;                                       (make-card 2 "Club"))
;                                      3)
;                          
;                                   
;                             "h")
;              (make-GS
;               (make-Base-State
;                #f
;                (list (make-card 2 "Club")
;                      (make-card 7 "Diamond")
;                      (make-card 5 "Club"))
;                #f
;                1
;                #f)
;               '()
;               3))
               
              

;; Base-State-Examples
;; hit => win => game-over 
;(check-expect (gs-transition (make-GS (make-Base-State #f
;                                                    (list (make-card 7 "Diamond")
;                                                          (make-card 5 "Club"))
;                                                    #f
;                                                    1
;                                                    #f)
;                                   (list
;                                    (make-card 9 "Club"))
;                                   3)
;                          "h")
;              (make-GS (make-Game-Over-State (list (make-card 7 "Diamond")
;                                                   (make-card 5 "Club")
;                                                   (make-card 9 "Club")))
;                       '()
;                       5))


;; dealers-cards : shoe -> shoe
;(define (dealers-cards shoe)
;  (cond
;    [(>= (add-list (flatten (cons (first shoe) (first (rest shoe))))) 17)
;     shoe]
;    [else #f]))
;
;(check-expect (dealers-cards (list (make-card 4 "Club")
;                                   (make-card 5 "Club")))
;              #f)


;; state-transition : state shoe wallet key -> GS
;; depending on `s` and `key, player's hand, shoe and wallet
(define (state-transition s shoe wallet key)
  (cond
    [(Base-State? s)
     (cond
       [(string=? key "h")
        (cond
          [(equal? (add-list (flatten
                              (cons (first shoe)
                                    (cons (Base-State-current-hand s)
                                          '())))) 21)
           ;; winning
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Base-State-current-hand s)
                                               '()))))
            (rest shoe)
            (+ wallet (* 2 (Base-State-bet s))))]
          ;; busting
          [( > (add-list (flatten
                          (cons (first shoe)
                                (cons (Base-State-current-hand s)
                                      '())))) 21)
           (cond
             [(equal? (Base-State-completed-split s) #false) 
              (make-GS (make-Round-Over-State (flatten
                                               (cons (first shoe)
                                                     (cons (Base-State-current-hand s)
                                                           '()))))
                       (rest shoe)
                       (- wallet (Base-State-bet s)))]
             [else
              (make-GS
               (make-Round-Over-State (flatten (list
                                                (Base-State-completed-split s)
                                                (flatten
                                                 (cons (first shoe)
                                                       (cons (Base-State-current-hand s)
                                                             '()))))))
               (rest shoe)
               (- wallet (Base-State-bet s)))])]
          [else 
           ;; not winning, not busting
           (make-GS
            (make-Base-State (Base-State-completed-split s)
                             (flatten
                              (cons (first shoe)
                                    (cons (Base-State-current-hand s)
                                          '())))
                             (Base-State-pending-split s)
                             (Base-State-bet s)
                             (Base-State-split-bet s))
            (rest shoe)
            wallet)])]
       ;; need to check players cards against dealers
       ;; right now, win if hand is greater than 17
       [(string=? key " ")
        (cond
          [(>= (add-list (Base-State-current-hand s)) 17)
           
           ;; holding and winning
           (make-GS
            (make-Game-Over-State (Base-State-current-hand s))
            shoe
            (+ wallet (* 2 (Base-State-bet s))))]
          ;; holding and losing
          [(< (add-list (Base-State-current-hand s)) 17)
           (cond
             [(equal? (Base-State-completed-split s) #false)
              (make-GS
               (make-Round-Over-State (Base-State-current-hand s))
               shoe
               (- wallet (Base-State-bet s)))]
             [else
              (make-GS
               (make-Round-Over-State (flatten (list
                                                (Base-State-completed-split s)
                                                (Base-State-current-hand s))))
               shoe
               (- wallet (Base-State-bet s)))])])])]
    ;; need a way to compare to the dealer
    [(Double-State? s)
     (cond
       [(string=? key "d")
        (cond
          [(equal? (add-list (flatten
                              (cons (first shoe)
                                    (cons (Double-State-current-hand s)
                                          '())))) 21)
           ;; winning
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Double-State-current-hand s)
                                               '()))))
            (rest shoe)
            (+ wallet (* 2 (Double-State-bet s))))]
          ;; lose
          [else
           ;; busting
           (make-GS
            (make-Round-Over-State (flatten
                                    (cons (first shoe)
                                          (cons (Double-State-current-hand s)
                                                '()))))
            (rest shoe)
            (- wallet (* 2 (Double-State-bet s))))])]

       [(string=? key " ")
        (make-GS
         (make-Base-State #f
                          (Double-State-current-hand s)
                          #f
                          (Double-State-bet s)
                          #f)
         shoe
         wallet)])] 
    [(Split-State? s)
     (cond
       [(string=? "s" key)
        (make-GS (make-Base-State #f
                                  (list (first (Split-State-current-hand s)))
                                  (rest (Split-State-current-hand s))
                                  (Split-State-bet s)
                                  (Split-State-bet s))
                 shoe
                 wallet)]
       [(string=? key " ")
        (make-GS
         (make-Base-State #f
                          (Split-State-current-hand s)
                          #f
                          (Split-State-bet s)
                          #f)
         shoe
         wallet)])]
    [(Split-Double-State? s)
     (cond
       [(string=? "d" key)
        (cond
          [(equal? (add-list (flatten
                              (cons (first shoe)
                                    (cons (Split-Double-State-current-hand s)
                                          '())))) 21)
           ;; winning
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Split-Double-State-current-hand s)
                                               '()))))
            (rest shoe)
            (+ wallet (* 2 (Split-Double-State-bet s))))]
          ;; lose
          [else
           ;; busting
           (make-GS
            (make-Round-Over-State (flatten
                                    (cons (first shoe)
                                          (cons (Split-Double-State-current-hand s)
                                                '()))))
            (rest shoe)
            (- wallet (* 2 (Split-Double-State-bet s))))])]
       [(string=? "s" key)
        (make-GS (make-Base-State #f
                                  (list (first (Split-Double-State-current-hand s)))
                                  (rest (Split-Double-State-current-hand s))
                                  (Split-Double-State-bet s)
                                  (Split-Double-State-bet s))
                 shoe
                 wallet)]
       [(string=? " " key)
        (make-GS
         (make-Base-State #f
                          (Split-Double-State-current-hand s)
                          #f
                          (Split-Double-State-bet s)
                          #f)
         shoe
         wallet)])]
    [(Round-Over-State? s)
     (make-GS
      (make-Round-Over-State (Round-Over-State-current-hand s))
      shoe
      wallet)]
    [(Game-Over-State? s)
     (make-GS
      (make-Game-Over-State (Game-Over-State-current-hand s))
      shoe
      wallet)]))

;; Tests as Examples:                                              
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 10 "Club")
                                                  (make-card 10 "Heart"))
                                                 (list
                                                  (make-card 10 "Diamond")
                                                  (make-card 7 "Heart"))
                                                 #f
                                                 3
                                                 3)
                                (list
                                 (make-card 6 "Club")
                                 (make-card 4 "Diamond"))
                                10
                                "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 10 "Club")
                                               (make-card 10 "Heart")
                                               (make-card 6 "Club")
                                               (make-card 10 "Diamond")
                                               (make-card 7 "Heart")))
                       (list (make-card 4 "Diamond"))
                       7))
                                           

;; hit => get 21 = win => game-over-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 5 "Spade"))
                                                 #f
                                                 1
                                                 #f)
                                (list
                                 (make-card 10 "Club")
                                 (make-card 4 "Heart"))
                                10
                                "h")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 6 "Diamond")
                                              (make-card 5 "Spade")))
                       (list
                        (make-card 4 "Heart"))
                       12))
;; hit => bust => lost
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 10 "Diamond")
                                                  (make-card 5 "Heart"))
                                                 #f
                                                 1
                                                 #f)
                                (list
                                 (make-card 10 "Club"))
                                10
                                "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 10 "Club")
                                               (make-card 10 "Diamond")
                                               (make-card 5 "Heart")))
                       '()
                       9))
              

;; hit => not win, not bust => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list (make-card 7 "Diamond")
                                                       (make-card 5 "Club"))
                                                 #f
                                                 1
                                                 #f)
                                (list
                                 (make-card 2 "Club"))
                                3
                                "h")
              (make-GS (make-Base-State #f
                                        (list (make-card 2 "Club")
                                              (make-card 7 "Diamond")
                                              (make-card 5 "Club"))
                                        #f
                                        1
                                        #f)
                       '()
                       3))

; hold => win => game-over
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 10 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 1
                                                 #f)
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Diamond")
                                              (make-card 10 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       12))

; hold => lose => round-over
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 2 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 1
                                                 #f)
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Round-Over-State (list
                                               (make-card 2 "Diamond")
                                               (make-card 10 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       9))

;; Double State Examples
;; double => win 
(check-expect (state-transition (make-Double-State 
                                 (list
                                  (make-card 5 "Diamond")
                                  (make-card 6 "Heart"))
                                 1)
                                (list
                                 (make-card 10 "Club"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 5 "Diamond")
                                              (make-card 6 "Heart")))
                       '()
                       12))

;; double => loss
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 3 "Diamond")
                                                    (make-card 9 "Heart"))
                                                   1)
                                (list
                                 (make-card 2 "Club"))
                                10
                                "d")
              (make-GS (make-Round-Over-State (list
                                               (make-card 2 "Club")
                                               (make-card 3 "Diamond")
                                               (make-card 9 "Heart")))
                       '()
                       8))

;; choose not to double
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 3 "Diamond")
                                                    (make-card 9 "Heart"))
                                                   1)
                                (list
                                 (make-card 2 "Club"))
                                10
                                " ")
              (make-GS (make-Base-State #f
                                        (list                  
                                         (make-card 3 "Diamond")
                                         (make-card 9 "Heart"))
                                        #f
                                        1
                                        #f)
                       (list
                        (make-card 2 "Club"))
                       10))




;; choose not to split 
(check-expect (state-transition (make-Split-State (list
                                                   (make-card 6 "Diamond")
                                                   (make-card 6 "Heart"))
                                                  1)
                                (list
                                 (make-card 2 "Club")
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                " ")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 6 "Diamond")
                                         (make-card 6 "Heart"))
                                        #f
                                        1
                                        #f)
                       (list
                        (make-card 2 "Club")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; choose to split
(check-expect (state-transition (make-Split-State (list
                                                   (make-card 6 "Diamond")
                                                   (make-card 6 "Heart"))
                                                  1)
                                (list
                                 (make-card 2 "Club")
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                "s")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 6 "Diamond"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 2 "Club")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; choose split 
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1)
                                (list
                                 (make-card 3 "Heart")
                                 (make-card 4 "Heart"))
                                10
                                "s")
              (make-GS (make-Base-State #f
                                        (list (make-card 5 "Club"))
                                        (list (make-card 5 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 3 "Heart")
                        (make-card 4 "Heart"))
                       10))

;; choose double => lose
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1)
                                (list
                                 (make-card 3 "Heart")
                                 (make-card 4 "Heart"))
                                10
                                "d")
              (make-GS (make-Round-Over-State (list
                                               (make-card 3 "Heart")
                                               (make-card 5 "Club")
                                               (make-card 5 "Heart")))
                       (list
                        (make-card 4 "Heart"))
                       8))
;; choose double => 21
(check-expect (state-transition (make-Split-Double-State (list
                                                          (make-card 9 "Heart")
                                                          (make-card 10 "Club"))
                                                         3)
                                (list
                                 (make-card 2 "Heart"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 2 "Heart")
                                              (make-card 9 "Heart")
                                              (make-card 10 "Club")))
                       '()
                       16))

;; no double, no split => base state
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1)
                                (list
                                 (make-card 3 "Heart")
                                 (make-card 4 "Heart"))
                                10
                                " ")
              (make-GS (make-Base-State #f
                                        (list (make-card 5 "Club")
                                              (make-card 5 "Heart"))
                                        #f
                                        1
                                        #f)
                       (list
                        (make-card 3 "Heart")
                        (make-card 4 "Heart"))
                       10))


;; choose to split: hit on first hand, dont bust => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1) 
                                (list
                                 (make-card 2 "Club")
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 2 "Club")
                                         (make-card 6 "Diamond"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; split => hit again on first split => not bust
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 2 "Club"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1)
                                (list
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 10 "Diamond")
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         )
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 10 "Heart"))
                       10))

;; split => win on first split, hit on second split => no bust => base state

(check-expect (state-transition (make-Base-State (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 2 "Club")
                                                  (make-card 10 "Diamond"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 #f
                                                 0
                                                 1)
                                (list
                                 (make-card 10 "Heart"))
                                12
                                "h")
              (make-GS (make-Base-State (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 10 "Diamond"))
                                        (list
                                         (make-card 10 "Heart")
                                         (make-card 6 "Heart"))
                                        #f
                                        0
                                        1)
                       '()
                       12))

; split => win on first split, hold on second split => no bust
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 2 "Club")
                                                  (make-card 10 "Diamond"))
                                                 (list
                                                  (make-card 6 "Heart")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 0
                                                 1)
                                '()
                                12
                                " ")
              (make-GS (make-Round-Over-State 
                        (list
                         (make-card 6 "Diamond")
                         (make-card 2 "Club")
                         (make-card 10 "Diamond")
                         (make-card 6 "Heart")
                         (make-card 10 "Heart")))      
                       '()
                       12))




;; choose to split: hit on first hand, dont bust => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1) 
                                (list
                                 (make-card 2 "Club")
                                 (make-card 8 "Spade")
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 2 "Club")
                                         (make-card 6 "Diamond"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 8 "Spade")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; split => hit again on first split => not bust => base state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 2 "Club"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1)
                                (list
                                 (make-card 8 "Spade")
                                 (make-card 10 "Diamond")
                                 (make-card 10 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 8 "Spade")
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))


;; split => lost on first split, hit on second split => no bust => base state
;; note : should bet = 0?
(check-expect (state-transition (make-Base-State 
                                 (list
                                  (make-card 6 "Diamond")
                                  (make-card 2 "Club")
                                  (make-card 8 "Spade"))
                                 (list
                                  (make-card 6 "Heart"))
                                 #f
                                 0
                                 1)
                                (list
                                 (make-card 10 "Heart")
                                 (make-card 10 "Diamond"))
                                9
                                "h")
              (make-GS (make-Base-State (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 8 "Spade"))
                                        (list
                                         (make-card 10 "Heart")
                                         (make-card 6 "Heart"))
                                        #f
                                        0
                                        1)
                       (list
                        (make-card 10 "Diamond"))
                       9))

;; split => lost first split, hit on second split => bust => round over state
;(check-expect (state-transition (make-Base-State (list
;                                                  (make-card 6 "Diamond")
;                                                  (make-card 2 "Club")
;                                                  (make-card 8 "Spade"))
;                                                 (list
;                                                  (make-card 6 "Heart")
;                                                  (make-card 10 "Diamond"))
;                                                 #f
;                                                 0
;                                                 1)
;                                (list
;                                 (make-card 10 "Heart"))
;                                9
;                                "h")
;              (make-GS (make-Round-Over-State (list
;                                               (make-card 6 "Diamond")
;                                               (make-card 2 "Club")
;                                               (make-card 8 "Spade")
;                                               (make-card 10 "Heart")
;                                               (make-card 6 "Heart")
;                                               (make-card 10 "Diamond")
;                                               ))
;                       '()
;                       8))


(check-expect (state-transition (make-Game-Over-State
                                 (list (make-card 2 "Club")
                                       (make-card 2 "Heart")))
                                '()
                                10
                                " ")
              (make-GS
               (make-Game-Over-State
                (list (make-card 2 "Club")
                      (make-card 2 "Heart")))
               '()
               10))

(check-expect (state-transition (make-Round-Over-State
                                 (list (make-card 2 "Club")
                                       (make-card 2 "Heart")))
                                '()
                                10
                                " ")
              (make-GS
               (make-Round-Over-State
                (list (make-card 2 "Club")
                      (make-card 2 "Heart")))
               '()
               10))

   
#|
PSA: not worked on yet

(define (start _dummy)
  (big-bang (make-fw (/ WORLD-WIDTH 2) "right" '() 0)
    [on-tick tick 1/200]
    [on-key  key]
    [to-draw draw]))

(big-bang GS0
  [on-tick tick 1/200]
  [on-key  key]
  [to-draw draw])

|#
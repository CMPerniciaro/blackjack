;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)

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

create invariants (eg bust invariant in base-state)

;; helper function that takes in state (what state am i in, and what key got pressed)

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

;(define-struct deck (LIST-OF-CARDS))
  
; A Wallet is:
; - Positive Number
; - '()
(define-struct wallet (amount))


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


;; functions = edges

(define LOC1 (cons (make-card 10 "Spade") (cons (make-card 11 "Diamond")'())))
(define LOC2 (list 1 2 3 4))



;; given a card, returns the value 


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






;; deal-cards : shoe -> LOC
;; deals cards
;;(define (deal-cards shoe)
  

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

;hit => bust => round-over

;; state-transition : state shoe wallet key -> GS
;; manipulates players hands based on key input
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
          ;; busting
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
     ;... (Game-Over-State-current-hand s) ...
     #f]))


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

;; choose double
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

;; no double, no split
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




   





;;(check-expect (transition (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))
;; (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))

;; need test case for each edge

;; bust-checker3 : GS -> GS
;; checks if LOC in GS goes over 21; if LOC > 21, make LOC '() and shoe '(),
;; else, return original GS
(define (bust-checker3 GS)
  (cond
    [(> (add-list (GS-shoe GS)) 21)
     (make-GS (make-Round-Over-State '())
              '() ;; need helper function to take out cards from shoe?
              (GS-wallet GS))]
    [else                           (make-GS
                                     (GS-state GS) ;;  do i need to specify base state
                                     (GS-shoe GS)
                                     (GS-wallet GS))]))

;(check-expect (bust-checker3 (make-GS (make-Base-State #f (list 11 11) #f) (list 11 11) 3))
;              (make-GS (make-Round-Over-State '()) '() 3))
;(check-expect (bust-checker3 (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))
;              (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))


;; double-checker : GS -> GS
;; checks for doubles; returns to og GS if no doubles, o.w. goes to Double-GS
(define (split-checker GS)
  (cond
    [(equal? (first (GS-shoe GS)) (rest (GS-shoe GS)))
     (make-GS
      (make-Split-State (GS-shoe GS))
      (GS-shoe GS) ;; need to double bet amount
      (GS-wallet GS))]
    [else
     (make-GS
      (GS-state GS) ;;  do i need to specify base state
      (GS-shoe GS)
      (GS-wallet GS))]))

;(check-expect (split-checker (make-GS (make-Base-State #f (list 1 2) #f) (list 1 2) 3))
;              (make-GS (make-Base-State #f (list 1 2) #f) (list 1 2) 3))

;; test fails because i need it to look like the splits are splitting but unsure 
;(check-expect (split-checker (make-GS (make-Base-State #f (list 7 7) #t) (list 7 7) 3))
;              (make-GS (make-Split-State (list 7)) (list 7) 3)) 


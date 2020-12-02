;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define-struct Base-State (completed-split current-hand pending-split bet split-bet))
;; define template for Base-State
#;
(define (base-state Base-State ...)
  ... (Base-State-completed-split Base-State) ...   ;LOC (if split completed)
  ;or #f if no split
  ... (Base-State-current-hand Base-State) ...      ;current hand of LOC
  ... (Base-State-pending-split Base-State) ...     ;LOC (if split) or #f
  ... (Base-State-bet Base-State) ...               ;current bet (Number)
  ... (Base-State-split-bet Base-State) ...)        ;number (if split) or #f


(define-struct Double-State (current-hand bet))
;; define template for Double-State
#;
(define (double-state Double-State ...)
  ... (Double-State-current-hand Double-State) ...  ;current LOC
  ... (Double-State-bet Double-State) ...)          ;current bet (number)

(define-struct Split-State (current-hand bet))
;; define template for Split-State
#;
(define (split-state Split-State ...)
  ... (Split-State-current-hand Split-State) ...  ;current LOC
  ... (Split-State-bet Split-State) ...)          ;current bet (number)

(define-struct Split-Double-State (current-hand bet))
;; define template for Split-Double-State
#;
(define (split-double-state Split-Double-State ...)
  ... (Split-Double-State-current-hand Split-Double-State) ...  ;current LOC(list)
  ... (Split-Double-State-bet Split-Double-State) ...)          ;current bet
;(number)

(define-struct Round-Over-State (current-hand))
;; define template for Round-Over-State
#;
(define (round-over-state Round-Over-State ...)
  ... (Round-Over-State-current-hand Round-Over-State) ...)   ;current LOC(list)

(define-struct Game-Over-State (current-hand))
;; define template for Game-Over-State
#;
(define (game-over-state Game-Over-State ...)
  ... (Game-Over-State-current-hand Game-Over-State) ...)       ;current LOC(list)


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

;; over-21? : LOC -> Boolean

;; base-state-checker : GS -> GS
;; determines the outcome of the incoming GS
(define (base-state-checker GS)
  (cond
    ;; hit/win
    [(equal? 21 (add-list (GS-state GS))) ;; not shoe, but not state?
     ;; go to game-over-state
     (make-GS
      (make-Game-Over-State
       (GS-shoe GS))
      (GS-shoe GS)
      (GS-wallet GS))]
    ;; hit/bust
    ;;[()]
    ;; hit/no bust
    [else #f]))

(check-expect (base-state-checker (make-GS (make-Base-State
                                            #f
                                            (list
                                             (make-card 10 "Club")
                                             (make-card 6 "Club")
                                             (make-card 5 "Club"))
                                            #f
                                            3
                                            #f)
                                           (list (make-card 7 "Club"))
                                           3))
              (make-GS
               (make-Game-Over-State (list
                                      (make-card 10 "Club")
                                      (make-card 6 "Club")
                                      (make-card 5 "Club")))
               
               (list (make-card 7 "Club"))
               3))
                
                                  
              
                                             

;; transition : GS key -> GS
;; takes in a GS and key, returns new GS depending on key
(define (transition GS key)
  (cond
    ;; hit
;    [(string=? key "h")
;     (base-state-checker GS)]
    [(make-GS (GS-state GS)
           (GS-state GS)
           (GS-wallet GS)) ]
    [(string=? key " ")
     #f]
    ;; double
    [(string=? key "d")
     #f]
    ;; split
    [(string=? key "s")
     #f]
    ;; nothing
    [else            GS]))

;; Examples

;; Base-State-Examples
;; hit => win => game-over 
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list (make-card 7 "Diamond")
                                                          (make-card 5 "Club"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card 9 "Club"))
                                   3)
                          "h")
              (make-GS (make-Game-Over-State (list (make-card 7 "Diamond")
                                                   (make-card 5 "Club")
                                                   (make-card 9 "Club")))
                       '()
                       5))

;hit => bust => round-over
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 10 "Diamond")
                                                     (make-card 5 "Heart"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card "Jack" "Club"))
                                   10)
                          "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 10 "Diamond")
                                               (make-card 5 "Heart")
                                               (make-card "Jack" "Club")))
                       '()
                       9))
              

;; hit => not win, not bust => base-state
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list (make-card 7 "Diamond")
                                                          (make-card 5 "Club"))
                                                    #f 1 #f)
                                   (list (make-card 2 "Club"))
                                   3) "h")
              (make-GS (make-Base-State #f
                                        (list (make-card 7 "Diamond")
                                              (make-card 5 "Club")
                                              (make-card 2 "Club"))
                                        #f 1 #f)
                       '()
                       3))
; hold => win => game-over
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 10 "Diamond")
                                                     (make-card 10 "Heart"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card "Jack" "Club"))
                                   10)
                          " ")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Diamond")
                                              (make-card 10 "Heart")))
                       (list
                        (make-card "Jack" "Club"))
                       12))

;; Double State Examples
;; double => win 
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 5 "Diamond")
                                                     (make-card 6 "Heart"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card "Jack" "Club"))
                                   10)
                          "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Diamond")
                                              (make-card 10 "Heart")
                                              (make-card "Jack" "Club")))
                       '()
                       12))

;; double => loss
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 5 "Diamond")
                                                     (make-card 6 "Heart"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card 2 "Club"))
                                   10)
                          "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Diamond")
                                              (make-card 10 "Heart")
                                              (make-card 2 "Club")))
                       '()
                       8))

;; Split Examples
; split : win split1, lose split2
;; get split => choose to split => base-state
(check-expect (transition (make-GS (make-Base-State #f
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
                                   10)
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

;; choose to split: hit on first hand, dont bust => base-state
(check-expect (transition (make-GS (make-Base-State #f
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
                                   10)
                          "h")
              (make-GS (make-Base-State #f
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
                       10))
;; split => hit again on first split => win
(check-expect (transition (make-GS (make-Base-State #f
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
                                   10)
                          "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 10 "Diamond"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 10 "Heart"))
                       12))
;; split => win on first split, hit on second split => no bust => base state
;; note : should bet = 0?
(check-expect (transition (make-GS (make-Base-State 
                                    (list
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
                                   12)
                          "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 10 "Diamond"))
                                        (list
                                         (make-card 6 "Heart")
                                         (make-card 10 "Heart"))
                                        0
                                        1)
                       '()
                       12))
;; split => win on first split, hold on second split => lose => round over
(check-expect (transition (make-GS (make-Base-State (list
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
                                   12)
                          " ")
              (make-GS (make-Round-Over-State 
                        (list
                         (make-card 6 "Diamond")
                         (make-card 2 "Club")
                         (make-card 10 "Diamond")
                         (make-card 6 "Heart")
                         (make-card 10 "Heart")))
                                  
                       '()
                       11))


;; split : lose both splits
;; choose to split => base-state
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 6 "Diamond")
                                                     (make-card 6 "Heart"))
                                                    #f
                                                    1
                                                    #f)
                                   (list
                                    (make-card 2 "Club")
                                    (make-card 8 "Spade")
                                    (make-card 10 "Diamond")
                                    (make-card 10 "Heart"))
                                   10)
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
                        (make-card 8 "Spade")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; choose to split: hit on first hand, dont bust => base-state
(check-expect (transition (make-GS (make-Base-State #f
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
                                   10)
                          "h")
              (make-GS (make-Base-State #f
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
                       10))
;; split => hit again on first split => not bust => base state
(check-expect (transition (make-GS (make-Base-State #f
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
                                   10)
                          "h")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 8 "Spade"))
                                        (list
                                         (make-card 6 "Heart"))
                                        1
                                        1)
                       (list
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       9))

;; split => hold on first split => lose first split
(check-expect (transition (make-GS (make-Base-State #f
                                                    (list
                                                     (make-card 6 "Diamond")
                                                     (make-card 2 "Club")
                                                     (make-card 8 "Spade"))
                                                    (list
                                                     (make-card 6 "Heart"))
                                                    0
                                                    1)
                                   (list
                                    (make-card 10 "Diamond")
                                    (make-card 10 "Heart"))
                                   9)
                          " ")
              (make-GS (make-Base-State (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 8 "Spade"))
                                        (list
                                         (make-card 6 "Heart"))
                                        #f
                                        0
                                        1)
                       (list
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       9))

;; split => lost on first split, hit on second split => no bust => base state
;; note : should bet = 0?
(check-expect (transition (make-GS (make-Base-State 
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
                                   9)
                          "h")
              (make-GS (make-Base-State (list
                                         (make-card 6 "Diamond")
                                         (make-card 2 "Club")
                                         (make-card 8 "Spade"))
                                        (list
                                         (make-card 6 "Heart")
                                         (make-card 10 "Diamond"))
                                        #f
                                        0
                                        1)
                       (list
                        (make-card 10 "Heart"))
                       9))

;; split => lost first split, hit on second split => bust => round over state
(check-expect (transition (make-GS (make-Base-State (list
                                                     (make-card 6 "Diamond")
                                                     (make-card 2 "Club")
                                                     (make-card 8 "Spade"))
                                                    (list
                                                     (make-card 6 "Heart")
                                                     (make-card 10 "Diamond"))
                                                    #f
                                                    0
                                                    1)
                                   (list
                                    (make-card 10 "Heart"))
                                   9)
                          "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 6 "Diamond")
                                               (make-card 2 "Club")
                                               (make-card 8 "Spade")
                                               (make-card 6 "Heart")
                                               (make-card 10 "Diamond")
                                               (make-card 10 "Heart")))
                       '()
                       8))




   





;;(check-expect (transition (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))
;; (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3))

;; need test case for each edge

; PC1 (hit bust)
;; bust-checker : LOC -> Boolean
;; checks if LOC is greater than 21
(define (bust-checker LOC)
  (cond
    [(> (add-list LOC) 21) #true]
    [else      #false]))

;(check-expect (bust-checker LOC2) #false)
;(check-expect (bust-checker (list 1 2 3 4 5 6 7)) #true)

;; bust-checker2 : GS -> Boolean
(define (bust-checker2 GS)
  (cond
    [(> (add-list (GS-shoe GS)) 21) #true]
    [else                          #false]))

;(check-expect (bust-checker2 (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3)) #f)
;(check-expect (bust-checker2 (make-GS (make-Base-State #f (list 11 11) #f) (list 11 11) 3)) #t)

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


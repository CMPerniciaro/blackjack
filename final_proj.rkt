;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


;; Data Definitions

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
         



;;;;;;;;;;;;;;;;;;;;;;;;
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

A GS is :
- (make-GS State LOC Number)
|#

;; add bets (great! names)

(define-struct GS (state shoe wallet))

(define-struct Base-State (completed-split current-hand pending-split bet split-bet))
(define-struct Double-State (current-hand bet))
(define-struct Split-State (current-hand bet))
(define-struct Split-Double-State (current-hand bet))
; no bets 
(define-struct Round-Over-State (current-hand))
(define-struct Game-Over-State (current-hand))

;; finish game state

;; functions = edges

;; transition : GS String -> GS

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

;; value-checker2 : GS -> Boolean
;; checks if LOC in GS is equal to 21
(define (value-checker2 GS)
  (cond
    [(equal? 21 (add-list (GS-shoe GS))) #true]
    [else        #false]))

;(check-expect (value-checker2 (make-GS (make-Base-State #f (list 1 2 3) #f) (list 1 2 3) 3)) #f)
;(check-expect (value-checker2 (make-GS (make-Base-State #f (list 10 11) #f) (list 10 11) 3)) #t)


;; transition : GS key-input -> GS
;; checks if LOC in GS is equal to 21; returns Game-Over-State if 21, else
;; returns original GS

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
    [(string=? key "h")
     (base-state-checker GS)]
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

;(check-expect (transition (make-GS (make-Base-State #f
;                                                    (list (make-card 7 "Diamond")
;                                                          (make-card 5 "Club"))
;                                                    #f 1 #f)
;                                   (list (make-card 9 "Club"))
;                                   3) "h")
;              (make-GS (make-Game-Over-State (list (make-card 7 "Diamond")
;                                                   (make-card 5 "Club")
;                                                   (make-card 9 "Club")))
;                       '()
;                       5))

;; hit, not win, not bust
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




;; dealer hits function

;(define (dealer-handler LOC)
;  (cond
;    [(>= add-list(LOC) 17) hold]
;    [(< add-list(LOC) 17) hit]
;    []))




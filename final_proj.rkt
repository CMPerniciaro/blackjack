;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Specification:

#|

UI Specifications:
- cards will have a flipping animation
- pre-determined bet amounts on the screen 
- keyboard controls to select bets, choose moves, etc.
- display wallet amount
- win and lose message/animation

Terms:
- Hit : take another card from the deck
- Hold : not take another card from the deck
- Bust : go over 21 and lose
- Blackjack : Having or getting your cards to equal to 21
- Ace : can equal a 1 or an 11 at the player's discretion
- Splitting Pairs : Player can split pairs when initial card are the same
                    denomination (i.e., two sixes, two jacks)
- Doubling Down : Player can double down when initial card dealt total to 9, 10,
                  or 11. A player can then double their original bet and be
                  dealt one more card. 

1. Player is given starting "wallet" amount for betting
2. Deck(s) are shuffled
3. Player makes initial bet
4. Player dealt 2 cards face up
5. Dealer dealt two cards, one face up, one face down
6. Having Blackjack right away:
   6a. Player has 21 right away:
       (6a.1) The dealer does not have 21 so the player will receive 1.5x the
              original bet from dealer
       (6a.2) The dealer also has 21 so the Player gets their original bet back
   6b. Dealer has 21 right away:
       (6b.1) The player does not have 21 right away so the dealer wins and
              takes the bets
       (6b.2) See (6a.2)
7. The Player can Hit or Hold
   7a. Hit 
       (7a.1) Bust
              (7a.4) If player's wallet is less than or equal to 0, the game
                     is over and the player loses 
       (7a.2) Get closer to 21
       (7a.3) The player wins and collects their original bet x2
   7b. Hold
       (7b.1) Player wins because they are the closest to 21 and collects x2
              their original bet 
       (7b.2) Lose because dealer was closer
              (7b.3) If player's wallet is less than or equal to 0, the game
                     is over and the player loses 
8. The Dealer can Hit or Hold
   8a. Dealer will hit until card total 17+
       (8a.1) Ace = 11 when it can total their card to 17+
   8b. Dealer will hold when cards total 17+
9. Player can Split pairs
   9a. Split (treat cards as two separate hands)
       (9a1) Original bet will be doubled and split across the two cards
       (9a2) Card1 will be played normally
             (9a2.1) See 7
       (9a3) Card2 will be played normally
             (9a3.1) See 7
   9b. Don't Split pairs
       (9b.1) Play normally (See 7)
10. Player can Double Down
    10a. Double down
       (10a1) Player doubles amount of original bet and is dealt one more card
       (10a2) Player then plays normally (See 7)
    10b. Don't double down
       (10b.1) Play normally (See 7)
|#

;; Data Definition (due in private meetings on Nov 19th)

#| Your data definition fleshes out your specification. It must contain:

   1. The data definition for your world and all its parts.

   2. A wishlist of functions that you expect you will need to write,
      each with a signature and purpose. (We expect this to be at least
      10 functions, but if you think carefully, you may have many more.
      Some will be obvious helpers for others. No trivial functions, however.)

The data definition meeting is your chance to get feedback on the essentials
of your design before you actually write lots of code. Because your instructors
are experienced designers, we are likely to have suggestions for you that will
improve your design. If you think carefully about the data definitions and get
feedback on them, it will be valuable to you and will save you more trouble
later.

Feel free to implement a few functions as a way to test out your data
definitions
the better quality data definitions you provide at this stage the better
feedback you will get and the less time the next step will take.

Before the meeting, push a file that has a clear and accurate listing of all of
the elements of the specification (in a comment) and the data definition.
This commit will affect the grading of the next section.
|#


;;(require games/cards)

;; Data Definitions

;; From library: returns a list of 52 cards, sorted lowest suit, then lowest
;; value
;(make-deck)

; A Deck is:
; - [List of Card]
; - '()
;(define-struct deck (LIST-OF-CARDS))

;; From library: returns a single card given a bitmap for the front, and values
;; for the card's suit and value
;(make-card)


; A Wallet is:
; - Positive Number
; - '()
(define-struct wallet (amount))

;; Possible Functions

;; shuffle-deck: List -> List
;; shuffles the deck
#|(define (shuffle-deck deck ...)
  ...)

;; flip-card: Card -> Card
;; performs the card animation
(define (flip-card card ...)
  ...)

;; hand-value-calculator: [List of Card] -> Positive Number
;; calculates the value of a hand 
(define (hand-value-calculator ...)
  ...)

;; wallet-update: Wallet -> Wallet
;; updates the wallet amount based on betting 
(define (wallet-update ...)
  ...)

;; bust-checker: [List of Card] -> Positive Number
;; checks if a hand is over 21
(define (bust-checker ...)
  ...)

;; blackjack-checker: [List of Card] -> Positive Number
;; checks if a hand is equal to 21
(define (blackjack-checker ...)
  ...)
|#
;; etc splitting, doubling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
    [()]
    ;; hit/no bust
    []))

(check-expect (base-state-checker (make-GS (make-Base-State
                                            #f
                                            (list
                                             (make-card 10 "Club")
                                             (make-card 6 "Club")
                                             (make-card 5 "Club"))
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




;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname final_proj) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/list)
(require 2htdp/image)
(require 2htdp/universe)

;; Data Definitions
         
;; A Game-State is :
;; - (make-GS State LOC Number)
(define-struct GS (state shoe wallet))

;; define template for GS
#;
(define (game-state GS ...)
  ... (GS-state GS) ...           ;the state of the same
  ... (GS-shoe GS)  ...           ;the list of cards that make up the deck
  ... (GS-wallet GS)   ...)          ;the player's money count (number)


(define-struct Base-State (completed-split current-hand pending-split bet
                                           split-bet dealers-hand))
(define-struct Double-State (current-hand bet dealers-hand))
(define-struct Split-State (current-hand bet dealers-hand))
(define-struct Split-Double-State (current-hand bet dealers-hand))
(define-struct Round-Over-State (current-hand dealers-hand))
(define-struct Game-Over-State (current-hand dealers-hand))

#|
A State is:
- (make-Base-State (LOC or #f) LOC (LOC or #f) Number (Number or #f) LOC  (PC1)

   INV: current-hand, pending-split cannot be bust;
        cannot have (LOC) LOC (LOC)
        cannot have #f LOC #f AND Number Number
        cannot have #f LOC LOC AND Number #f
        if pending-split is LOC <=> then bet 2 is Number 


- (make-Double-State LOC Number LOC)                           (PC2)
- (make-Split-State LOC Number LOC)                            (PC3)
- (make-Split-Double-State LOC Number LOC)                     (PC4)
- (make-Round-Over-State LOC LOC)
- (make-Game-Over-State LOC LOC)
- "start"

|#

#;
(define (state-template s ...)
  (cond
    [(Base-State? s)   ... (Base-State-completed-split s) ...   
                       ... (Base-State-current-hand s) ...      
                       ... (Base-State-pending-split s) ...     
                       ... (Base-State-bet s)    ...               
                       ... (Base-State-split-bet s) ...
                       ... (Base-State-dealers-hand s) ...]
    [(Double-State? s) ... (Double-State-current-hand s) ...  
                       ... (Double-State-bet s) ...
                       ... (Dealer-State-dealers-hand s)...]
    [(Split-State? s)  ... (Split-State-current-hand s) ...  
                       ... (Split-State-bet s) ...
                       ... (Split-State-dealers-hand s) ...]
    [(Split-Double-State? s)  ... (Split-Double-State-current-hand s) ...  
                              ... (Split-Double-State-bet s) ...
                              ... (Split-Double-State-dealers-hand s)...]
    [(Round-Over-State? s) ... (Round-Over-State-current-hand s) ...
                           ... (Round-Over-State-dealers-hand s)]
    [(Game-Over-State? s)  ... (Game-Over-State-current-hand s) ...
                           ... (Game-Over-State-dealers-hand s)]
    [(string=? "start" s)...]))

; A Shoe is:
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


(define GS0 (make-GS
             (make-Base-State #f
                              (list
                               (make-card 6 "Diamond")
                               (make-card 5 "Spade"))
                              #f
                              1
                              #f
                              (list
                               (make-card 4 "Club")))
             (list (make-card 10 "Club")
                   (make-card 3 "Heart"))
             10))

(define GS1 (make-GS
             (make-Base-State #f
                              (list
                               (make-card 3 "Diamond")
                               (make-card 5 "Spade"))
                              #f
                              1
                              #f
                              (list
                               (make-card 4 "Club")))
             (list (make-card 3 "Club")
                   (make-card 3 "Heart"))
             10))

(define GS2 (make-GS
             (make-Double-State (list (make-card 4 "Spade")
                                      (make-card 5 "Spade"))
                                1
                                (list (make-card 10 "Club")
                                      (make-card 5 "Club")))
             (list (make-card 3 "Club")
                   (make-card 3 "Heart"))
             100))

(define GS3 (make-GS
             (make-Split-State (list (make-card 4 "Spade")
                                     (make-card 4 "Heart"))
                               1
                               (list (make-card 10 "Club")
                                     (make-card 5 "Club")))
             (list (make-card 3 "Club")
                   (make-card 3 "Heart"))
             100))

;; decks does not include ace or face cards
(define list-club (list (make-card 2 "Club")
                        (make-card 3 "Club")
                        (make-card 4 "Club")
                        (make-card 5 "Club")
                        (make-card 6 "Club")
                        (make-card 7 "Club")
                        (make-card 8 "Club")
                        (make-card 9 "Club")
                        (make-card 10 "Club")))
(define list-spade (list (make-card 10 "Club")
                         (make-card 2 "Spade")
                         (make-card 3 "Spade")
                         (make-card 4 "Spade")
                         (make-card 5 "Spade")
                         (make-card 6 "Spade")
                         (make-card 7 "Spade")
                         (make-card 8 "Spade")
                         (make-card 9 "Spade")
                         (make-card 10 "Spade")))

(define list-heart (list (make-card 2 "Heart")
                         (make-card 3 "Heart")
                         (make-card 4 "Heart")
                         (make-card 5 "Heart")
                         (make-card 6 "Heart")
                         (make-card 7 "Heart")
                         (make-card 8 "Heart")
                         (make-card 9 "Heart")
                         (make-card 10 "Heart")))

(define list-diamond (list (make-card 2 "Diamond")
                           (make-card 3 "Diamond")
                           (make-card 4 "Diamond")
                           (make-card 5 "Diamond")
                           (make-card 6 "Diamond")
                           (make-card 7 "Diamond")
                           (make-card 8 "Diamond")
                           (make-card 9 "Diamond")
                           (make-card 10 "Diamond")))

(define list-deck (list list-club
                        list-spade
                        list-heart
                        list-diamond))
                         

(define decks (flatten (list list-deck
                             list-deck
                             list-deck
                             list-deck
                             list-deck
                             list-deck))) 

(define CARD-NUMBER-SIZE 25)
(define CARD-WIDTH-SIZE 50)
(define CARD-HEIGHT-SIZE 100)
(define CARD-OUTLINE-COLOR "white")
(define PLAYER-CARD-X-POS-START 30)
(define PLAYER-CARD-Y-POS-START1 330)
(define PLAYER-CARD-Y-POS-START2 440)
(define DEALERS-CARD-X-POS-START 470)
(define DEALERS-CARD-Y-POS-START 55)
(define FONT-SIZE 16)
(define MOVE-X-BY 60)
(define WALLET-BET-X-POS 470)
(define WALLET-Y-POS 480)
(define BET-Y-POS 440)
(define SPLIT-BET-Y-POS 460)
(define SCENE-WIDTH 500)
(define SCENE-HEIGHT 500)

;; Diamond Image
(define diamond-img
  (rhombus 15 45 "solid" "red"))

;; Club Image
(define club-img
  (overlay/offset
   (overlay/offset
    (circle 7 "solid" "black")
    0
    10
    (overlay/offset
     (circle 7 "solid" "black")
     13 0
     (circle 7 "solid" "black")))
   0
   12
   (triangle 14 "solid" "black")))

;; Heart Image
(define heart-img
  (overlay/offset
   (rotate 22
           diamond-img)
   10
   0
   (rotate 160
           diamond-img)))
;; spade image
(define spade-img
  (overlay/offset 
   (flip-vertical
    (overlay/offset
     (rotate 22
             (rhombus 15 45 "solid" "black"))
     10
     0
     (rotate 160
             (rhombus 15 45 "solid" "black"))))
   0
   9
   (triangle 15 "solid" "black")))

(define BACKGROUND
  (overlay/offset
   (text "Press 1 to bet $10" FONT-SIZE "black")
   -180
   -20
   (overlay/offset
    (text "Press 5 to bet $5" FONT-SIZE "black")
    -180
    0
    (overlay/offset 
     (overlay/offset  
      (overlay/offset
       (text "Wallet" FONT-SIZE "black")
       0
       -20
       (text "Split Bet" FONT-SIZE "black"))
      0
      -30
      (text "Bet" FONT-SIZE "black")) 
     -175
     -210
     (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "green")))))

(define BASE-STATE-BACKGROUND
  (overlay/offset
   (text "space to hold" FONT-SIZE "black")
   -200
   -20
   (overlay/offset
    (text "h to hit" FONT-SIZE "black")
    -200
    0
    (overlay/offset 
     (overlay/offset  
      (overlay/offset
       (text "Wallet" FONT-SIZE "black")
       0
       -20
       (text "Split Bet" FONT-SIZE "black"))
      0
      -30
      (text "Bet" FONT-SIZE "black")) 
     -175
     -210
     (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "green")))))

(define DOUBLE-STATE-BACKGROUND
  (overlay/offset
   (text "space to not double" FONT-SIZE "black")
   -170
   -20
   (overlay/offset
    (text "d to double" FONT-SIZE "black")
    -200
    0
    (overlay/offset 
     (overlay/offset  
      (overlay/offset
       (text "Wallet" FONT-SIZE "black")
       0
       -20
       (text "Split Bet" FONT-SIZE "black"))
      0
      -30
      (text "Bet" FONT-SIZE "black")) 
     -175
     -210
     (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "green")))))

(define SPLIT-STATE-BACKGROUND
  (overlay/offset
   (text "space to not split" FONT-SIZE "black")
   -170
   -20
   (overlay/offset
    (text "s to split" FONT-SIZE "black")
    -200
    0
    (overlay/offset 
     (overlay/offset  
      (overlay/offset
       (text "Wallet" FONT-SIZE "black")
       0
       -20
       (text "Split Bet" FONT-SIZE "black"))
      0
      -30
      (text "Bet" FONT-SIZE "black")) 
     -175
     -210
     (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "green")))))

(define SPLIT-DOUBLE-STATE-BACKGROUND
  (overlay/offset
   (text "space for normal play" FONT-SIZE "black")
   -170
   20
   (overlay/offset
    (text "s to split" FONT-SIZE "black")
    -190
    -20
    (overlay/offset
     (text "d to double" FONT-SIZE "black")
     -200
     0
     (overlay/offset 
      (overlay/offset  
       (overlay/offset
        (text "Wallet" FONT-SIZE "black")
        0
        -20
        (text "Split Bet" FONT-SIZE "black"))
       0
       -30
       (text "Bet" FONT-SIZE "black")) 
      -175
      -210
      (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "green"))))))

(define start-blackjack
  (make-GS "start"
           (shuffle decks)
           100))

;; card-image-creator : String String Image -> Image
;; takes in value, color, and suit of card and returns an image
;; Strategy: functional composition
(define (card-image-creator number color suit-img)
  (overlay
   (text number CARD-NUMBER-SIZE color)
   (overlay/align "left" "top"
                  suit-img
                  (overlay/align "right" "bottom"
                                 suit-img
                                 (rectangle
                                  CARD-WIDTH-SIZE
                                  CARD-HEIGHT-SIZE
                                  "solid"
                                  CARD-OUTLINE-COLOR)))))

;; draw : GS -> Image
;; takes in GS and returns an Image
;; Strategy: structural decomposition
(define (draw GS)
  (place-images
   (flatten (list (draw-num (GS-wallet GS))))
   (list 
    (make-posn WALLET-BET-X-POS WALLET-Y-POS))
   (state-to-image (GS-state GS) (GS-shoe GS))))

(check-expect (draw (make-GS (make-Base-State #f
                                              (list (make-card 2 "Club")
                                                    (make-card 4 "Heart"))
                                              #f
                                              1
                                              #f
                                              (list (make-card 3 "Club")))
                             (list (make-card 6 "Heart"))
                             10))
              
              (place-images
               (list 
                (card-image-creator "2" "black" club-img)
                (card-image-creator "4" "red" heart-img)
                (card-image-creator "3" "black" club-img)
                (text "1" 16 "black")
                (text "10" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)
                     (make-posn WALLET-BET-X-POS 480))
               BASE-STATE-BACKGROUND))

(check-expect (draw (make-GS (make-Base-State
                              (list (make-card 3 "Heart")
                                    (make-card 4 "Spade"))
                              (list (make-card 2 "Club")
                                    (make-card 4 "Heart"))
                              #f
                              1
                              0
                              (list (make-card 3 "Club")))
                             (list (make-card 6 "Heart"))
                             10))
              
              (place-images
               (list 
                (card-image-creator "2" "black" club-img)
                (card-image-creator "4" "red" heart-img)
                (card-image-creator "3" "red" heart-img)
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "3" "black" club-img)
                (text "1" 16 "black")
                (text "10" 16 "black")
                (text "0" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 30 330)
                     (make-posn 90 330)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)
                     (make-posn WALLET-BET-X-POS 480)
                     (make-posn WALLET-BET-X-POS SPLIT-BET-Y-POS))
               BASE-STATE-BACKGROUND))

;; create-posn-player : [List-of Cards] Number Number-> [List-of Posn]
;; takes in [List-of Cards], Number, Number and returns [List-of Posn]
;; to create as many positions needed for cards being drawn
;; Strategy: functional composition
(define (create-posn-player LOC posx posy) 
  (cond
    [(empty? LOC) LOC]
    [else     (cons (make-posn posx posy)
                    (create-posn-player (rest LOC)
                                        (+ posx MOVE-X-BY) posy))]))

(check-expect (create-posn-player (list (make-card 4 "Spade")
                                        (make-card 5 "Spade")) 
                                  30
                                  440)
              (list (make-posn 30 440)
                    (make-posn 90 440)))

(check-expect (create-posn-player (list (make-card 4 "Spade")
                                        (make-card 5 "Spade")
                                        (make-card 7 "Heart")) 
                                  30
                                  440)
              (list (make-posn 30 440)
                    (make-posn 90 440)
                    (make-posn 150 440)))

;; create-posn-dealer : [List-of Cards] Number Number-> [List-of Posn]
;; takes in [List-of Cards], Number, Number and returns [List-of Posn]
;; to create as many positions needed for cards being drawn
;; Strategy: functional composition
(define (create-posn-dealer LOC posx posy) 
  (cond
    [(empty? LOC) LOC]
    [else     (cons (make-posn posx posy)
                    (create-posn-dealer (rest LOC)
                                        (- posx MOVE-X-BY) posy))]))

(check-expect (create-posn-dealer (list (make-card 4 "Spade")
                                        (make-card 5 "Spade")) 
                                  470
                                  55)
              (list (make-posn 470 55)
                    (make-posn 410 55)))

;; state-to-image : s shoe -> Image
;; takes in a state and shoe and returns an image of that state
;; Strategy: structural decomp/functional composition
(define (state-to-image s shoe)
  (cond
    [(Base-State? s)
     (cond
       ;; no splits
       [(and (equal? (Base-State-completed-split s) #false)
             (equal? (Base-State-pending-split s) #false))
        (place-images
         (flatten (list (loc-to-loi (Base-State-current-hand s))
                        (card-to-image (first (Base-State-dealers-hand s)))
                        (draw-num (Base-State-bet s))))
         (flatten (list (create-posn-player
                         (Base-State-current-hand s)
                         PLAYER-CARD-X-POS-START
                         PLAYER-CARD-Y-POS-START2)
                        (make-posn DEALERS-CARD-X-POS-START
                                   DEALERS-CARD-Y-POS-START)
                        (make-posn WALLET-BET-X-POS BET-Y-POS)))
         BASE-STATE-BACKGROUND)]
       ;; have completed split
       [(and (list? (Base-State-completed-split s))
             (equal? (Base-State-pending-split s) #false))
        (place-images
         (flatten (list (loc-to-loi (Base-State-completed-split s))
                        (loc-to-loi (Base-State-current-hand s))
                        (card-to-image (first (Base-State-dealers-hand s)))
                        (draw-num (Base-State-bet s))
                        (draw-num (Base-State-split-bet s))))
         (flatten
          (list (create-posn-player
                 (Base-State-completed-split s)
                 PLAYER-CARD-X-POS-START
                 PLAYER-CARD-Y-POS-START1)
                (create-posn-player
                 (Base-State-current-hand s)
                 PLAYER-CARD-X-POS-START
                 PLAYER-CARD-Y-POS-START2)
                (make-posn DEALERS-CARD-X-POS-START DEALERS-CARD-Y-POS-START)
                (make-posn WALLET-BET-X-POS BET-Y-POS)
                (make-posn WALLET-BET-X-POS SPLIT-BET-Y-POS)))
         BASE-STATE-BACKGROUND)]
       ;; have pending split 
       [else
        (place-images
         (flatten (list (loc-to-loi (Base-State-current-hand s))
                        (loc-to-loi (Base-State-pending-split s))
                        (card-to-image (first (Base-State-dealers-hand s)))
                        (draw-num (Base-State-bet s))
                        (draw-num (Base-State-split-bet s))))
         (flatten
          (list (create-posn-player
                 (Base-State-current-hand s)
                 PLAYER-CARD-X-POS-START
                 PLAYER-CARD-Y-POS-START1)
                (create-posn-player
                 (Base-State-pending-split s)
                 PLAYER-CARD-X-POS-START
                 PLAYER-CARD-Y-POS-START2)
                (make-posn DEALERS-CARD-X-POS-START DEALERS-CARD-Y-POS-START)
                (make-posn WALLET-BET-X-POS BET-Y-POS)
                (make-posn WALLET-BET-X-POS SPLIT-BET-Y-POS)))
         BASE-STATE-BACKGROUND)])]
    [(Double-State? s)
     (place-images
      (flatten (list (loc-to-loi (Double-State-current-hand s))
                     (card-to-image (first (Double-State-dealers-hand s)))
                     (draw-num (Double-State-bet s))))
      (flatten (list (create-posn-player
                      (Double-State-current-hand s)
                      PLAYER-CARD-X-POS-START
                      PLAYER-CARD-Y-POS-START2)
                     (make-posn DEALERS-CARD-X-POS-START
                                DEALERS-CARD-Y-POS-START)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)))
      DOUBLE-STATE-BACKGROUND)]
    [(Split-State? s)
     (place-images
      (flatten (list (loc-to-loi (Split-State-current-hand s)) 
                     (card-to-image (first (Split-State-dealers-hand s)))
                     (draw-num (Split-State-bet s))))
      (flatten (list (create-posn-player
                      (Split-State-current-hand s)
                      PLAYER-CARD-X-POS-START
                      PLAYER-CARD-Y-POS-START2)
                     (make-posn DEALERS-CARD-X-POS-START
                                DEALERS-CARD-Y-POS-START)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)))
      SPLIT-STATE-BACKGROUND)]
    [(Split-Double-State? s)
     (place-images
      (flatten (list (loc-to-loi (Split-Double-State-current-hand s))
                     (card-to-image (first
                                     (Split-Double-State-dealers-hand s)))
                     (draw-num (Split-Double-State-bet s))))
      (flatten (list (create-posn-player
                      (Split-Double-State-current-hand s)
                      PLAYER-CARD-X-POS-START
                      PLAYER-CARD-Y-POS-START2)
                     (make-posn DEALERS-CARD-X-POS-START
                                DEALERS-CARD-Y-POS-START)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)))
      SPLIT-DOUBLE-STATE-BACKGROUND)]
    [(Round-Over-State? s)
     (place-images
      (flatten (list (loc-to-loi (Round-Over-State-current-hand s))
                     (loc-to-loi (Round-Over-State-dealers-hand s))))
      (flatten (list (create-posn-player
                      (Round-Over-State-current-hand s)
                      PLAYER-CARD-X-POS-START
                      PLAYER-CARD-Y-POS-START2)
                     (create-posn-dealer
                      (Round-Over-State-dealers-hand s)
                      DEALERS-CARD-X-POS-START
                      DEALERS-CARD-Y-POS-START)))
      BACKGROUND)]
    [(Game-Over-State? s)
     (place-images
      (flatten (list (loc-to-loi (Game-Over-State-current-hand s))
                     (loc-to-loi (Game-Over-State-dealers-hand s))))
      (flatten (list (create-posn-player
                      (Game-Over-State-current-hand s) 
                      PLAYER-CARD-X-POS-START
                      PLAYER-CARD-Y-POS-START2)
                     (create-posn-dealer
                      (Game-Over-State-dealers-hand s)
                      DEALERS-CARD-X-POS-START
                      DEALERS-CARD-Y-POS-START)))
      BACKGROUND)]
    [(string=? "start" s)
     (cond
       [(or (equal? (add-list (list (first shoe)
                                    (cadr shoe))) 9)
            (equal? (add-list (list (first shoe)
                                    (cadr shoe))) 10)
            (equal? (add-list (list (first shoe)
                                    (cadr shoe))) 11))
        (place-images
         (flatten (list (card-to-image (first shoe))
                        (card-to-image (cadr shoe))))
         (flatten (list (make-posn PLAYER-CARD-X-POS-START
                                   PLAYER-CARD-Y-POS-START2)
                        (make-posn (+ PLAYER-CARD-X-POS-START MOVE-X-BY)
                                   PLAYER-CARD-Y-POS-START2)))
         BACKGROUND)]
       [(equal? (card-value (first shoe)) 
                (card-value (cadr shoe)))
        (place-images
         (flatten (list (card-to-image (first shoe))
                        (card-to-image (cadr shoe))))
         (flatten (list (make-posn PLAYER-CARD-X-POS-START
                                   PLAYER-CARD-Y-POS-START2)
                        (make-posn (+ PLAYER-CARD-X-POS-START MOVE-X-BY)
                                   PLAYER-CARD-Y-POS-START2)))
         BACKGROUND)]
       [(and (equal? (card-value (first shoe)) 5)
             (equal? (card-value (cadr shoe)) 5))
        (place-images
         (flatten (list (card-to-image (first shoe))
                        (card-to-image (cadr shoe))))
         (flatten (list (make-posn PLAYER-CARD-X-POS-START
                                   PLAYER-CARD-Y-POS-START2)
                        (make-posn (+ PLAYER-CARD-X-POS-START MOVE-X-BY)
                                   PLAYER-CARD-Y-POS-START2)))
         BACKGROUND)]
       [else
        (place-images
         (flatten (list (card-to-image (first shoe))
                        (card-to-image (cadr shoe))))
         (flatten (list (make-posn PLAYER-CARD-X-POS-START
                                   PLAYER-CARD-Y-POS-START2)
                        (make-posn (+ PLAYER-CARD-X-POS-START MOVE-X-BY)
                                   PLAYER-CARD-Y-POS-START2)))
         BACKGROUND)])]))

;; no splits
(check-expect (state-to-image (make-Base-State #f
                                               (list (make-card 4 "Spade")
                                                     (make-card 6 "Diamond"))
                                               #f
                                               1
                                               #f
                                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "6" "red" diamond-img)
                (card-image-creator "5" "black" spade-img)
                (text "1" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS))
               BASE-STATE-BACKGROUND))

(check-expect (state-to-image (make-Base-State #f
                                               (list (make-card 4 "Spade")
                                                     (make-card 6 "Diamond")
                                                     (make-card 7 "Spade"))
                                               #f
                                               1
                                               #f
                                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "6" "red" diamond-img)
                (card-image-creator "7" "black" spade-img)
                (card-image-creator "5" "black" spade-img)
                (text "1" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 150 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS))
               BASE-STATE-BACKGROUND))

;; completed split and current hand
(check-expect (state-to-image (make-Base-State (list (make-card 10 "Spade")
                                                     (make-card 3 "Spade"))
                                               (list (make-card 4 "Spade")
                                                     (make-card 6 "Spade"))
                                               #f
                                               1
                                               1
                                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list
                (card-image-creator "10" "black" spade-img)
                (card-image-creator "3" "black" spade-img)
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "6" "black" spade-img)
                (card-image-creator "5" "black" spade-img)
                (text "1" 16 "black")
                (text "1" 16 "black"))
               (list (make-posn 30 PLAYER-CARD-Y-POS-START1) 
                     (make-posn 90 PLAYER-CARD-Y-POS-START1)
                     (make-posn 30 PLAYER-CARD-Y-POS-START2)
                     (make-posn 90 PLAYER-CARD-Y-POS-START2)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)
                     (make-posn WALLET-BET-X-POS SPLIT-BET-Y-POS))
               BASE-STATE-BACKGROUND))
;; pending split and current hand
(check-expect (state-to-image (make-Base-State #f
                                               (list (make-card 4 "Spade"))
                                               (list (make-card 4 "Heart"))
                                               1
                                               1
                                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "4" "red" heart-img)
                (card-image-creator "5" "black" spade-img)
                (text "1" 16 "black")
                (text "1" 16 "black"))
               (list (make-posn 30 330)
                     (make-posn 30 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS)
                     (make-posn WALLET-BET-X-POS SPLIT-BET-Y-POS))
               BASE-STATE-BACKGROUND))

;; double state
(check-expect (state-to-image (make-Double-State (list (make-card 4 "Spade")
                                                       (make-card 5 "Spade"))
                                                 1
                                                 (list (make-card 4 "Heart")
                                                       (make-card 5 "Heart")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "5" "black" spade-img)
                (card-image-creator "4" "red" heart-img)
                (text "1" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS))
               DOUBLE-STATE-BACKGROUND))

;; split state
(check-expect (state-to-image (make-Split-State (list (make-card 4 "Spade")
                                                      (make-card 4 "Club"))
                                                1
                                                (list (make-card 4 "Heart")
                                                      (make-card 5 "Heart")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "4" "black" spade-img)
                (card-image-creator "4" "black" club-img)
                (card-image-creator "4" "red" heart-img)
                (text "1" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS))
               SPLIT-STATE-BACKGROUND))

;; split double
(check-expect (state-to-image (make-Split-Double-State
                               (list (make-card 5 "Spade")
                                     (make-card 5 "Club"))
                               1
                               (list (make-card 4 "Heart")
                                     (make-card 5 "Heart")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "5" "black" spade-img)
                (card-image-creator "5" "black" club-img)
                (card-image-creator "4" "red" heart-img)
                (text "1" 16 "black"))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn 470 55)
                     (make-posn WALLET-BET-X-POS BET-Y-POS))
               SPLIT-DOUBLE-STATE-BACKGROUND))

(check-expect (state-to-image (make-Round-Over-State
                               (list (make-card 2 "Club")
                                     (make-card 10 "Club"))
                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "2" "black" club-img)
                (card-image-creator "10" "black" club-img)
                (card-image-creator "5" "black" spade-img))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn DEALERS-CARD-X-POS-START
                                DEALERS-CARD-Y-POS-START))
               BACKGROUND))

(check-expect (state-to-image (make-Game-Over-State
                               (list (make-card 2 "Club")
                                     (make-card 10 "Club"))
                               (list (make-card 5 "Spade")))
                              (shuffle decks))
              (place-images
               (list 
                (card-image-creator "2" "black" club-img)
                (card-image-creator "10" "black" club-img)
                (card-image-creator "5" "black" spade-img))
               (list (make-posn 30 440)
                     (make-posn 90 440)
                     (make-posn DEALERS-CARD-X-POS-START
                                DEALERS-CARD-Y-POS-START))
               BACKGROUND))

;; draw-bet : Number -> Image
;; takes in a number and returns and Image
;; Strategy: functional composition
(define (draw-num num)
  (text (number->string num) FONT-SIZE "black"))

(check-expect (draw-num 10)
              (text "10" FONT-SIZE "black"))

;; card-to-image : Card -> Image
;; takes in a card and returns an image
;; Strategy: functional composition
(define (card-to-image card)
  (cond
    [(eq? (card-suit card) "Club")
     (card-image-creator (number->string (card-value card))
                         "black"
                         club-img)]
    [(eq? (card-suit card) "Spade")
     (card-image-creator (number->string (card-value card))
                         "black"
                         spade-img)]
    [(eq? (card-suit card) "Heart")
     (card-image-creator (number->string (card-value card))
                         "red"
                         heart-img)]
    [else
     (card-image-creator (number->string (card-value card))
                         "red"
                         diamond-img)]))

(check-expect (card-to-image (make-card 2 "Spade"))
              (card-image-creator "2" "black" spade-img))
(check-expect (card-to-image (make-card 2 "Club"))
              (card-image-creator "2" "black" club-img))
(check-expect (card-to-image (make-card 2 "Heart"))
              (card-image-creator "2" "red" heart-img))
(check-expect (card-to-image (make-card 2 "Diamond"))
              (card-image-creator "2" "red" diamond-img))

;; loc-to-loi : [List-of Cards] -> [List-of Images]
;; take in a [List-of Cards] and return a [List-of Images]
;; Strategy: functional composition
(define (loc-to-loi LOC)
  (cond
    [(empty? LOC) LOC]
    [(eq? (card-suit (first LOC)) "Club")
     (cons (card-image-creator (number->string (card-value (first LOC)))
                               "black"
                               club-img)
           (loc-to-loi (rest LOC)))]
    [(eq? (card-suit (first LOC)) "Spade")
     (cons (card-image-creator (number->string (card-value (first LOC)))
                               "black"
                               spade-img)
           (loc-to-loi (rest LOC)))]
    [(eq? (card-suit (first LOC)) "Heart")
     (cons (card-image-creator (number->string (card-value (first LOC)))
                               "red"
                               heart-img)
           (loc-to-loi (rest LOC)))]
    [else
     (cons (card-image-creator (number->string (card-value (first LOC)))
                               "red"
                               diamond-img)
           (loc-to-loi (rest LOC)))]))

(check-expect (loc-to-loi (list (make-card 2 "Club")))
              (list (card-image-creator "2" "black" club-img)))
(check-expect (loc-to-loi (list (make-card 2 "Spade")))
              (list (card-image-creator "2" "black" spade-img)))
(check-expect (loc-to-loi (list (make-card 2 "Heart")))
              (list (card-image-creator "2" "red" heart-img)))
(check-expect (loc-to-loi (list (make-card 2 "Diamond")))
              (list (card-image-creator "2" "red" diamond-img)))
(check-expect (loc-to-loi (list (make-card 2 "Club")
                                (make-card 3 "Club")
                                (make-card 4 "Heart")))
              (list (card-image-creator "2" "black" club-img)
                    (card-image-creator "3" "black" club-img)
                    (card-image-creator "4" "red" heart-img)))
(check-expect (loc-to-loi (list (make-card 2 "Spade")
                                (make-card 3 "Club")
                                (make-card 4 "Heart")))
              (list (card-image-creator "2" "black" spade-img)
                    (card-image-creator "3" "black" club-img)
                    (card-image-creator "4" "red" heart-img)))
(check-expect (loc-to-loi (list (make-card 2 "Diamond")
                                (make-card 3 "Club")
                                (make-card 4 "Heart")))
              (list (card-image-creator "2" "red" diamond-img)
                    (card-image-creator "3" "black" club-img)
                    (card-image-creator "4" "red" heart-img)))
(check-expect (loc-to-loi (list (make-card 2 "Heart")
                                (make-card 3 "Club")
                                (make-card 4 "Heart")))
              (list (card-image-creator "2" "red" heart-img)
                    (card-image-creator "3" "black" club-img)
                    (card-image-creator "4" "red" heart-img)))

;; add-list : [List-of Cards] -> Number
;; adds up the elements of a [List-of Cards]
;; Strategy: structural decomposition
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

;; gs-transition : GS key -> GS
;; takes in a GS and key, returns new GS depending on key
;; Strategy : structural decomposition
(define (gs-transition GS key)
  (state-transition (GS-state GS)            
                    (GS-shoe GS)             
                    (GS-wallet GS)
                    key))

(check-expect (gs-transition (make-GS (make-Double-State
                                       (list
                                        (make-card 4 "Spade")
                                        (make-card 5 "Spade"))
                                       2
                                       (list
                                        (make-card 10 "Heart")))
                                      (list
                                       (make-card 3 "Diamond")
                                       (make-card 7 "Club"))
                                      10)
                             " ")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 4 "Spade")
                                         (make-card 5 "Spade"))
                                        #f
                                        2
                                        #f
                                        (list
                                         (make-card 10 "Heart")))
                       (list
                        (make-card 3 "Diamond")
                        (make-card 7 "Club"))
                       10))
                                                               
;; state-transition : s shoe wallet key -> GS
;; depending on `s` and `key, returns specific GS
;; Strategy: structural decomp
(define (state-transition s shoe wallet key)
  (cond
    [(Base-State? s)
     (cond
       ;; base-state: hit
       [(string=? key "h")
        (cond
          ;; equal 21? => win (done) 
          [(equal? (add-list (flatten
                              (cons (first shoe)
                                    (cons (Base-State-current-hand s)
                                          '())))) 21)
           (cond
             ;; T1a : win on current hand and have no splits
             [(and (equal? (Base-State-completed-split s) #false)
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Game-Over-State (flatten
                                      (cons (first shoe)
                                            (cons (Base-State-current-hand s)
                                                  '())))
                                     (Base-State-dealers-hand s))
               (rest shoe)
               (+ wallet (* 2 (Base-State-bet s))))]
             ;; T1b: win on current hand and have completed split
             [(and (list? (Base-State-completed-split s))
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Game-Over-State
                (flatten
                 (cons (first shoe)
                       (cons (Base-State-current-hand s)
                             (cons
                              (Base-State-completed-split s)
                              '()))))
                (Base-State-dealers-hand s))
               (rest shoe)
               (+ wallet (* 2 (Base-State-bet s))))]
             ;; T1c: win on current and still have pending
             [else
              (make-GS
               (make-Base-State   (flatten
                                   (cons (first shoe)
                                         (cons (Base-State-current-hand s)
                                               '())))
                                  (Base-State-pending-split s)
                                  #f
                                  (Base-State-split-bet s)
                                  0
                                  (Base-State-dealers-hand s))
               (rest shoe)
               (+ wallet (* 2(Base-State-bet s))))])]
          ;; cards>21? => bust = lost (done)
          [( > (add-list (flatten
                          (cons (first shoe)
                                (cons (Base-State-current-hand s)
                                      '())))) 21)
           (cond
             ;; T2a: if not in split state
             [(and (equal? (Base-State-completed-split s) #false)
                   (equal? (Base-State-pending-split s) #false)) 
              (make-GS (make-Round-Over-State
                        (flatten
                         (cons (first shoe)
                               (cons (Base-State-current-hand s)
                                     '())))
                        (Base-State-dealers-hand s))
                       (rest shoe)
                       (- wallet (Base-State-bet s)))]
             ;; T2b: lose on current hand and have completed split => round over
             [(and (list? (Base-State-completed-split s))
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Round-Over-State
                (flatten
                 (cons (first shoe)
                       (cons (Base-State-current-hand s)
                             (cons
                              (Base-State-completed-split s)
                              '()))))
                                      (Base-State-dealers-hand s))
               (rest shoe)
               (- wallet (Base-State-bet s)))]
             ;; T2c: lose on current, still have pending => base-state 
             [else
              (make-GS
               (make-Base-State  (flatten
                                  (cons (first shoe)
                                        (cons (Base-State-current-hand s)
                                              '())))
                                 (Base-State-pending-split s)
                                 #f
                                 (Base-State-split-bet s)
                                 0
                                 (Base-State-dealers-hand s))
               (rest shoe)
               (- wallet (Base-State-bet s)))])]
          ;; T3c: LOC < 21 => base-state
          [else 
           (make-GS
            (make-Base-State (Base-State-completed-split s)
                             (flatten
                              (cons (first shoe)
                                    (cons (Base-State-current-hand s)
                                          '())))
                             (Base-State-pending-split s)
                             (Base-State-bet s)
                             (Base-State-split-bet s)
                             (Base-State-dealers-hand s))
            (rest shoe)
            wallet)])]
       ;; base-state: hold 
       [(string=? key " ")
        (cond
          ;; player LOC > dealer LOC => win 
          [(> (add-list (Base-State-current-hand s))
              (add-list (Base-State-dealers-hand s)))
           (cond
             ;; T4a: if not in split state
             [(and (equal? (Base-State-completed-split s) #false)
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Game-Over-State (Base-State-current-hand s)
                                     (Base-State-dealers-hand s))
               shoe
               (+ wallet (* 2 (Base-State-bet s))))]
             ;; T4b: win on current hand and have completed split => game-over
             [(and (list? (Base-State-completed-split s))
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Game-Over-State (flatten
                                      (list
                                       (Base-State-current-hand s) 
                                       (Base-State-completed-split s)
                                       ))
                                     (Base-State-dealers-hand s))
               shoe
               (+ wallet (* 2 (Base-State-bet s))))]
             ;; T4c: win on current hand and have pending split => base-state
             [else
              (make-GS
               (make-Base-State   (Base-State-current-hand s)
                                  (Base-State-pending-split s)
                                  #f
                                  (Base-State-split-bet s)
                                  0
                                  (Base-State-dealers-hand s))
               shoe
               (+ wallet (* 2 (Base-State-bet s))))])]
          ;; player LOC < dealer LOC => lose
          [(< (add-list (Base-State-current-hand s))
              (add-list (Base-State-dealers-hand s)))
           (cond
             ;; T5a: did not come from split state
             [(and (equal? (Base-State-completed-split s) #false)
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Round-Over-State (Base-State-current-hand s)
                                      (Base-State-dealers-hand s))
               shoe
               (- wallet (Base-State-bet s)))]
             ;; T5b: lose on current hand and have completed split => game-over
             [(and (list? (Base-State-completed-split s))
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Round-Over-State (flatten (list
                                                (Base-State-completed-split s)
                                                (Base-State-current-hand s)))
                                      (Base-State-dealers-hand s))
               shoe
               (- wallet (Base-State-bet s)))]
             ;; T5c: lose on current hand and have pending split
             [else
              (make-GS
               (make-Base-State  (Base-State-current-hand s)
                                 (Base-State-pending-split s)
                                 #f
                                 (Base-State-split-bet s)
                                 0
                                 (Base-State-dealers-hand s))
               shoe
               (- wallet (Base-State-bet s)))])]
          ;; player LOC = dealer LOC => draw 
          [(equal? (add-list (Base-State-current-hand s))
                   (add-list (Base-State-dealers-hand s)))
           (cond
             ;; T6a: not in split
             [(and (equal? (Base-State-completed-split s) #false)
                   (equal? (Base-State-pending-split s) #false))
              (make-GS
               (make-Game-Over-State (Base-State-current-hand s)
                                     (Base-State-dealers-hand s))
               shoe
               wallet)]
             ;; T6b: draw on current hand and have completed split
             [(and (list? (Base-State-completed-split s))
                   (equal? (Base-State-pending-split s) #false))
              (make-GS (make-Game-Over-State (flatten
                                              (list
                                               (Base-State-completed-split s)
                                               (Base-State-current-hand s)))
                                             (Base-State-dealers-hand s))
                       shoe
                       wallet)]
             ;; T6c: draw on current hand and have pending split
             [else
              (make-GS
               (make-Base-State  (Base-State-current-hand s)
                                 (Base-State-pending-split s)
                                 #f
                                 (Base-State-split-bet s)
                                 0
                                 (Base-State-dealers-hand s))
               shoe
               wallet)])])])]
    ;; double-state
    [(Double-State? s)
     (cond
       ;; hit
       [(string=? key "d")
        (cond
          ;; T7a: player LOC = 21 => win 
          [(or (equal? (add-list (flatten
                                  (cons (first shoe)
                                        (cons (Double-State-current-hand s)
                                              '())))) 21)
               ;; T7b: player LOC > dealer LOC
               (> (add-list (flatten
                             (cons (first shoe)
                                   (cons (Double-State-current-hand s)
                                         '()))))
                  (add-list (Double-State-dealers-hand s))))
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Double-State-current-hand s)
                                               '())))
                                  (Double-State-dealers-hand s))
            (rest shoe)
            (+ wallet (* 2 (Double-State-bet s))))]
          ;; T7c: player LOC < dealer LOC
          [(< (add-list (flatten
                         (cons (first shoe)
                               (cons (Double-State-current-hand s)
                                     '()))))
              (add-list (Double-State-dealers-hand s)))
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Double-State-current-hand s)
                                               '())))
                                  (Double-State-dealers-hand s))
            (rest shoe)
            (- wallet (* 2 (Double-State-bet s))))]
          ;; T7d: player LOC = dealer LOC => draw
          [(equal? (add-list (flatten
                              (cons (first shoe)
                                    (cons (Double-State-current-hand s)
                                          '()))))
                   (add-list (Double-State-dealers-hand s)))
           (make-GS
            (make-Game-Over-State (flatten
                                   (cons (first shoe)
                                         (cons (Double-State-current-hand s)
                                               '())))
                                  (Double-State-dealers-hand s))
            (rest shoe)
            wallet)])]
       ;; T8: don't double
       [(string=? key " ")
        (make-GS
         (make-Base-State #f
                          (Double-State-current-hand s)
                          #f
                          (Double-State-bet s)
                          #f
                          (Double-State-dealers-hand s))
         shoe
         wallet)])] 
    [(Split-State? s)
     (cond
       ;; T9: split
       [(string=? "s" key)
        (make-GS (make-Base-State #f
                                  (list
                                   (first (Split-State-current-hand s)))
                                  (rest (Split-State-current-hand s))
                                  (Split-State-bet s)
                                  (Split-State-bet s)
                                  (Split-State-dealers-hand s))
                 shoe
                 wallet)]
       ;; T10: don't split
       [(string=? key " ")
        (make-GS
         (make-Base-State #f
                          (Split-State-current-hand s)
                          #f
                          (Split-State-bet s)
                          #f
                          (Split-State-dealers-hand s))
         shoe
         wallet)])] 
    [(Split-Double-State? s)
     (cond
       ;; T11a: double
       [(string=? "d" key)
        (make-GS
         (make-Double-State (Split-Double-State-current-hand s)
                            (Split-Double-State-bet s)
                            (Split-Double-State-dealers-hand s))
         shoe
         wallet)]
       ;; T11b: split
       [(string=? "s" key)
        (make-GS (make-Base-State
                  #f
                  (list (first (Split-Double-State-current-hand s)))
                  (rest (Split-Double-State-current-hand s))
                  (Split-Double-State-bet s)
                  (Split-Double-State-bet s)
                  (Split-Double-State-dealers-hand s))
         shoe
         wallet)]
       ;; T11c: play normal
       [(string=? " " key)
        (make-GS
         (make-Base-State #f
                          (Split-Double-State-current-hand s)
                          #f
                          (Split-Double-State-bet s)
                          #f
                          (Split-Double-State-dealers-hand s))
         shoe
         wallet)])]
    ;; T12
    [(Round-Over-State? s)
     (make-GS "start"
              shoe
              wallet)]
    ;; T13
    [(Game-Over-State? s)
     (make-GS "start"
              shoe
              wallet)]
    [(string=? "start" s)
     (cond
       [(string=? "5" key) 
        (cond
          ;; T14a: double 
          [(and (or (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 9)
               (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 10)
               (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 11))
                (or (not (equal? (card-value (first shoe)) 5))
                     (not (equal? (card-value (cadr shoe)) 5))))
           (make-GS
            (make-Double-State (list (first shoe)
                                     (cadr shoe))
                               5
                               (list
                                (first (rest (rest shoe)))
                                (first (rest (rest (rest shoe)))))) 
            (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)]
          ;; T14b: split
          [(and (equal? (card-value (first shoe))
                   (card-value (cadr shoe)))
                (not (equal? (card-value (first shoe)) 5)))
           (make-GS (make-Split-State
                     (list (first shoe)
                           (cadr shoe))
                     5
                     (list
                      (first (rest (rest shoe)))
                      (first (rest (rest (rest shoe))))))
            (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)]
          ;; T14c: split-double
          [(and (equal? (card-value (first shoe)) 5)
                (equal? (card-value (cadr shoe)) 5))
           (make-GS (make-Split-Double-State
                     (list (first shoe)
                           (cadr shoe))
                     5
                     (list
                      (first (rest (rest shoe)))
                      (first (rest (rest (rest shoe))))))
           (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)]
          ;; T15d: base-state
          [else
           (make-GS
            (make-Base-State #f
                             (list (first shoe)
                                   (cadr shoe))
                             #f
                             5
                             #f
                             (list
                              (first (rest (rest shoe)))
                              (first (rest (rest (rest shoe))))))
           (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)])]
       [(string=? "1" key)
        (cond
          ;; double
          [(and (or (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 9)
               (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 10)
               (equal? (add-list (list (first shoe)
                                       (cadr shoe))) 11))
                (or (not (equal? (card-value (first shoe)) 5))
                     (not (equal? (card-value (cadr shoe)) 5))))
           (make-GS
            (make-Double-State (list (first shoe)
                                     (cadr shoe))
                               10
                               (list
                                (first (rest (rest shoe)))
                                (first (rest (rest (rest shoe)))))) 
            (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)]
          ;; split
          [(and (equal? (card-value (first shoe))
                   (card-value (cadr shoe)))
                (not (equal? (card-value (first shoe)) 5)))
           (make-GS (make-Split-State (list (first shoe)
                                            (cadr shoe))
                                      10
                                      (list
                                       (first (rest (rest shoe)))
                                       (first (rest (rest (rest shoe))))))
                    (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
                    wallet)]
          [(and (equal? (card-value (first shoe)) 5)
                (equal? (card-value (cadr shoe)) 5))
           (make-GS (make-Split-Double-State (list (first shoe)
                                                   (cadr shoe))
                                             10
                                           (list
                                            (first (rest (rest shoe)))
                                            (first (rest (rest (rest shoe))))))
                    (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
                    wallet)] 
          [else
           (make-GS
            (make-Base-State #f
                             (list (first shoe)
                                   (cadr shoe))
                             #f
                             10
                             #f
                             (list
                              (first (rest (rest shoe)))
                              (first (rest (rest (rest shoe))))))
            (remove (first (rest (rest (rest shoe))))
                    (remove (first (rest (rest shoe)))
                    (remove (cadr shoe) (remove (first shoe)(rest shoe)))))
            wallet)])])]))




;; Tests as Examples:                                              
;; T1a: BS | (no splits) "h" => 21 = win
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 5 "Spade"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 4 "Club")))
                                (list
                                 (make-card 10 "Club")
                                 (make-card 4 "Heart"))
                                10
                                "h")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 6 "Diamond")
                                              (make-card 5 "Spade"))
                                             (list (make-card 4 "Club")))
                       (list
                        (make-card 4 "Heart"))
                       12))

;; T1b: BS | (splits in completed and current) "h" => 21 = win
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 10 "Heart")
                                                  (make-card 11 "Heart"))
                                                 (list
                                                  (make-card 10 "Diamond")
                                                  (make-card 9 "Spade"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 4 "Club")))
                                (list
                                 (make-card 2 "Club")
                                 (make-card 4 "Heart"))
                                10
                                "h")
              (make-GS (make-Game-Over-State (list
                                              (make-card 2 "Club")
                                              (make-card 10 "Diamond")
                                              (make-card 9 "Spade")
                                              (make-card 10 "Heart")
                                              (make-card 11 "Heart"))
                                             (list
                                              (make-card 4 "Club")))
                       (list
                        (make-card 4 "Heart"))
                       12))
;; T1c: BS | (splits in current and pending) "h" => 21 = win => base
(check-expect (state-transition (make-Base-State  #f
                                                  (list
                                                   (make-card 10 "Heart")
                                                   (make-card 9 "Heart")) 
                                                  (list
                                                   (make-card 10 "Diamond"))
                                                  1
                                                  1
                                                  (list
                                                   (make-card 4 "Club")))
                                (list
                                 (make-card 2 "Club")
                                 (make-card 4 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State  (list    (make-card 2 "Club")
                                                  (make-card 10 "Heart")
                                                  (make-card 9 "Heart"))
                                         (list
                                          (make-card 10 "Diamond"))
                                         #f
                                         1
                                         0
                                         (list
                                          (make-card 4 "Club")))
                       (list
                        (make-card 4 "Heart"))
                       12))

;; T2a: BS | (no split) "h" => bust => lost
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 10 "Diamond")
                                                  (make-card 5 "Heart"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 7 "Diamond")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 10 "Club")
                                               (make-card 10 "Diamond")
                                               (make-card 5 "Heart"))
                                              (list
                                               (make-card 7 "Diamond")))
                       '()
                       9))

;; T2b: BS | (split current and completed) "h" => bust => lost
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 7 "Club")
                                                  (make-card 10 "Club")
                                                  (make-card 5 "Club"))
                                                 (list
                                                  (make-card 7 "Diamond")
                                                  (make-card 9 "Heart"))
                                                 #f
                                                 1
                                                 0
                                                 (list
                                                  (make-card 8 "Diamond")))
                                (list
                                 (make-card 10 "Heart"))
                                10
                                "h")
              (make-GS (make-Round-Over-State (list
                                               (make-card 10 "Heart")
                                               (make-card 7 "Diamond")
                                               (make-card 9 "Heart")
                                               (make-card 7 "Club")
                                               (make-card 10 "Club")
                                               (make-card 5 "Club"))
                                              (list
                                               (make-card 8 "Diamond")))
                       '()
                       9))

;; T2c: BS | (split) "h" => bust => lost
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 9 "Diamond")
                                                  (make-card 5 "Heart"))
                                                 (list
                                                  (make-card 9 "Heart"))
                                                 1
                                                 1
                                                 (list
                                                  (make-card 7 "Diamond")))
                                (list
                                 (make-card 10 "Club")
                                 (make-card 2 "Club")
                                 (make-card 3 "Club"))
                                10
                                "h")
              (make-GS (make-Base-State   (list
                                           (make-card 10 "Club")
                                           (make-card 9 "Diamond")
                                           (make-card 5 "Heart"))
                                          (list
                                           (make-card 9 "Heart"))
                                          #f
                                          1
                                          0
                                          (list
                                           (make-card 7 "Diamond")))
                       (list
                        (make-card 2 "Club")
                        (make-card 3 "Club"))
                       9))

;; T3: LOC < 21 => can play again = base state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 5 "Spade"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 4 "Club")))
                                (list
                                 (make-card 2 "Club")
                                 (make-card 4 "Heart"))
                                10
                                "h")
              (make-GS (make-Base-State   #f
                                          (list
                                           (make-card 2 "Club")
                                           (make-card 6 "Diamond")
                                           (make-card 5 "Spade"))
                                          #f
                                          1
                                          #f
                                          (list
                                           (make-card 4 "Club")))
                       (list
                        (make-card 4 "Heart"))
                       10))


;; T4a: BS | " " => (no splits) win => game-over-state
(check-expect (state-transition (make-Base-State #f
                                                 (list (make-card 7 "Diamond")
                                                       (make-card 5 "Club"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 6 "Heart")))
                                (list
                                 (make-card 2 "Club"))
                                3
                                " ")
              (make-GS (make-Game-Over-State  (list (make-card 7 "Diamond")
                                                    (make-card 5 "Club"))
                                              (list
                                               (make-card 6 "Heart")))
                       (list
                        (make-card 2 "Club"))
                       5))

;; T4b: BS | " " => (split completed hand, current hand) "h" => win, not bust
;; => game over
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 7 "Heart"))
                                                 (list (make-card 7 "Diamond")
                                                       (make-card 10 "Club"))
                                                 #f
                                                 1
                                                 0
                                                 (list
                                                  (make-card 6 "Heart")))
                                (list
                                 (make-card 4 "Club"))
                                3
                                " ")
              (make-GS (make-Game-Over-State (list 
                                              (make-card 7 "Diamond")
                                              (make-card 10 "Club")
                                              (make-card 7 "Heart"))
                                             (list
                                              (make-card 6 "Heart")))
                       (list
                        (make-card 4 "Club"))
                       5))

;; T4c: BS | " " => (split pending hand, current hand) => win => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 10 "Club")
                                                  (make-card 9 "Diamond"))
                                                 (list
                                                  (make-card 9 "Heart"))
                                                 1
                                                 1
                                                 (list
                                                  (make-card 7 "Diamond")))
                                (list
                                 (make-card 2 "Club")
                                 (make-card 3 "Club"))
                                10
                                " ")
              (make-GS (make-Base-State   (list
                                           (make-card 10 "Club")
                                           (make-card 9 "Diamond"))
                                          (list
                                           (make-card 9 "Heart"))
                                          #f
                                          1
                                          0
                                          (list
                                           (make-card 7 "Diamond")))
                       (list
                        (make-card 2 "Club")
                        (make-card 3 "Club"))
                       12))

;; T5a: BS | " " (no split) => lose => round-over
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 2 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 1
                                                 #f
                                                 (list
                                                  (make-card 7 "Club")
                                                  (make-card 7 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Round-Over-State (list
                                               (make-card 2 "Diamond")
                                               (make-card 10 "Heart"))
                                              (list
                                               (make-card 7 "Club")
                                               (make-card 7 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       9))

;; T5b: BS | " " (completed split, current hand) => lose => game-over
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 2 "Diamond")
                                                  (make-card 10 "Heart")
                                                  (make-card 3 "Diamond"))
                                                 (list
                                                  (make-card 7 "Heart")
                                                  (make-card 3 "Club"))
                                                 #f
                                                 1
                                                 0
                                                 (list
                                                  (make-card 7 "Club")
                                                  (make-card 7 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Round-Over-State (list
                                               (make-card 2 "Diamond")
                                               (make-card 10 "Heart")
                                               (make-card 3 "Diamond")
                                               (make-card 7 "Heart")
                                               (make-card 3 "Club"))
                                              (list
                                               (make-card 7 "Club")
                                               (make-card 7 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       9))

;; T5c: BS | " " (pending split, current hand) => lose => round-over
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 8 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 (list
                                                  (make-card 8 "Heart"))
                                                 1
                                                 1
                                                 (list
                                                  (make-card 10 "Club")
                                                  (make-card 9 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Base-State (list
                                         (make-card 8 "Diamond")
                                         (make-card 10 "Heart"))
                                        (list
                                         (make-card 8 "Heart"))
                                        #f
                                        1
                                        0
                                        (list
                                         (make-card 10 "Club")
                                         (make-card 9 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       9))

;; T6a: BS | " " (no split) => draw => game-over
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 2 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 3
                                                 #f
                                                 (list
                                                  (make-card 10 "Spade")
                                                  (make-card 2 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Game-Over-State (list
                                              (make-card 2 "Diamond")
                                              (make-card 10 "Heart"))
                                             (list
                                              (make-card 10 "Spade")
                                              (make-card 2 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       10))

;; T6b: BS | " " (completed split, current hand) => draw => game-over
(check-expect (state-transition (make-Base-State (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 (list
                                                  (make-card 2 "Diamond")
                                                  (make-card 10 "Heart"))
                                                 #f
                                                 4
                                                 0
                                                 (list
                                                  (make-card 10 "Spade")
                                                  (make-card 2 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Game-Over-State (list
                                              (make-card 6 "Diamond")
                                              (make-card 10 "Heart")
                                              (make-card 2 "Diamond")
                                              (make-card 10 "Heart"))
                                             (list
                                              (make-card 10 "Spade")
                                              (make-card 2 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       10))

;; T6c: BS | " " => (split pending hand, current hand) => draw => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 10 "Club")
                                                  (make-card 2 "Diamond"))
                                                 (list
                                                  (make-card 10 "Heart"))
                                                 2
                                                 1
                                                 (list
                                                  (make-card 7 "Diamond")
                                                  (make-card 5 "Spade")))
                                (list
                                 (make-card 2 "Club")
                                 (make-card 3 "Club"))
                                10
                                " ")
              (make-GS (make-Base-State   (list
                                           (make-card 10 "Club")
                                           (make-card 2 "Diamond"))
                                          (list
                                           (make-card 10 "Heart"))
                                          #f
                                          1
                                          0
                                          (list
                                           (make-card 7 "Diamond")
                                           (make-card 5 "Spade")))
                       (list
                        (make-card 2 "Club")
                        (make-card 3 "Club"))
                       10))

;; Double State Examples
;; T7a: player LOC = 21
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 5 "Diamond")
                                                    (make-card 6 "Heart"))
                                                   1
                                                   (list
                                                    (make-card 2 "Spade")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 5 "Diamond")
                                              (make-card 6 "Heart"))
                                             (list
                                              (make-card 2 "Spade")))
                       '()
                       12))

;; T7b: player LOC > dealers LOC => win
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 5 "Diamond")
                                                    (make-card 4 "Heart"))
                                                   1
                                                   (list
                                                    (make-card 4 "Spade")
                                                    (make-card 10 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 5 "Diamond")
                                              (make-card 4 "Heart"))
                                             (list
                                              (make-card 4 "Spade")
                                              (make-card 10 "Heart")))
                       '()
                       12))
;; T7c: player LOC < dealers LOC => lose
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 5 "Diamond")
                                                    (make-card 4 "Heart"))
                                                   1
                                                   (list
                                                    (make-card 10 "Spade")
                                                    (make-card 10 "Heart")))
                                (list
                                 (make-card 4 "Club"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 4 "Club")
                                              (make-card 5 "Diamond")
                                              (make-card 4 "Heart"))
                                             (list
                                              (make-card 10 "Spade")
                                              (make-card 10 "Heart")))
                       '()
                       8))
;; T7d: player LOC = dealers LOC => draw
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 5 "Diamond")
                                                    (make-card 4 "Heart"))
                                                   1
                                                   (list
                                                    (make-card 10 "Spade")
                                                    (make-card 9 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                "d")
              (make-GS (make-Game-Over-State (list
                                              (make-card 10 "Club")
                                              (make-card 5 "Diamond")
                                              (make-card 4 "Heart"))
                                             (list
                                              (make-card 10 "Spade")
                                              (make-card 9 "Heart")))
                       '()
                       10))

;; T8: don't double
(check-expect (state-transition (make-Double-State (list
                                                    (make-card 5 "Diamond")
                                                    (make-card 4 "Heart"))
                                                   1
                                                   (list
                                                    (make-card 10 "Spade")
                                                    (make-card 9 "Heart")))
                                (list
                                 (make-card 10 "Club"))
                                10
                                " ")
              (make-GS (make-Base-State #f
                                        (list
                                         (make-card 5 "Diamond")
                                         (make-card 4 "Heart"))
                                        #f
                                        1
                                        #f
                                        (list
                                         (make-card 10 "Spade")
                                         (make-card 9 "Heart")))
                       (list
                        (make-card 10 "Club"))
                       10))
;; T10: do not split
(check-expect (state-transition (make-Split-State (list
                                                   (make-card 6 "Diamond")
                                                   (make-card 6 "Heart"))
                                                  1
                                                  (list
                                                   (make-card 4 "Spade")))
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
                                        #f
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 2 "Club")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; T9: split
(check-expect (state-transition (make-Split-State (list
                                                   (make-card 6 "Diamond")
                                                   (make-card 6 "Heart"))
                                                  1
                                                  (list
                                                   (make-card 4 "Spade")))
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
                                        1
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 2 "Club")
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;; T11a: choose double
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1
                                 (list
                                  (make-card 4 "Spade")))
                                (list
                                 (make-card 3 "Heart")
                                 (make-card 4 "Heart"))
                                10
                                "d")
              (make-GS (make-Double-State (list (make-card 5 "Club")
                                                (make-card 5 "Heart"))
                                          1
                                          (list
                                           (make-card 4 "Spade")))
                       (list
                        (make-card 3 "Heart")
                        (make-card 4 "Heart"))
                       10))
;; T11b: choose split 
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1
                                 (list
                                  (make-card 4 "Spade")))
                                (list
                                 (make-card 3 "Heart")
                                 (make-card 4 "Heart"))
                                10
                                "s")
              (make-GS (make-Base-State #f
                                        (list (make-card 5 "Club"))
                                        (list (make-card 5 "Heart"))
                                        1
                                        1
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 3 "Heart")
                        (make-card 4 "Heart"))
                       10))

;; T11c: play normal (no split, no double)
(check-expect (state-transition (make-Split-Double-State
                                 (list (make-card 5 "Club")
                                       (make-card 5 "Heart"))
                                 1
                                 (list
                                  (make-card 4 "Spade")))
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
                                        #f
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 3 "Heart")
                        (make-card 4 "Heart"))
                       10))

;; T12
(check-expect (state-transition (make-Round-Over-State
                                 (list (make-card 2 "Club")
                                       (make-card 2 "Heart"))
                                 (list (make-card 5 "Diamond")))
                                '()
                                10
                                " ")
              (make-GS "start"
                       '()
                       10))
;; T13
(check-expect (state-transition (make-Game-Over-State
                                 (list (make-card 2 "Club")
                                       (make-card 2 "Heart"))

                                 (list (make-card 5 "Spade")))
                                '()
                                10
                                " ")
              (make-GS "start"
                       '()
                       10))
;; T14a
(check-expect (state-transition "start"
                                (list
                                 (make-card 4 "Club")
                                 (make-card 5 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "5")
              (make-GS
               (make-Double-State (list (make-card 4 "Club")
                                       (make-card 5 "Heart"))
                                 5
                                 (list (make-card 5 "Spade")
                                       (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))
;; T14b
(check-expect (state-transition "start"
                                (list
                                 (make-card 10 "Club")
                                 (make-card 10 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "5")
              (make-GS
               (make-Split-State (list (make-card 10 "Club")
                                       (make-card 10 "Heart"))
                                 5
                                 (list (make-card 5 "Spade")
                                       (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))
;; T14c
(check-expect (state-transition "start"
                                (list
                                 (make-card 5 "Club")
                                 (make-card 5 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "5")
              (make-GS
               (make-Split-Double-State (list (make-card 5 "Club")
                                              (make-card 5 "Heart"))
                                        5
                                        (list (make-card 5 "Spade")
                                              (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))

;; T14d
(check-expect (state-transition "start"
                                (list
                                 (make-card 3 "Club")
                                 (make-card 2 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "5")
              (make-GS
               (make-Base-State #f
                                (list (make-card 3 "Club")
                                      (make-card 2 "Heart"))
                                #f
                                5
                                #f
                                (list (make-card 5 "Spade")
                                      (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))


(check-expect (state-transition "start"
                                (list
                                 (make-card 4 "Club")
                                 (make-card 5 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "1")
              (make-GS
               (make-Double-State (list (make-card 4 "Club")
                                       (make-card 5 "Heart"))
                                 10
                                 (list (make-card 5 "Spade")
                                       (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))

(check-expect (state-transition "start"
                                (list
                                 (make-card 10 "Club")
                                 (make-card 10 "Heart")
                                 (make-card 5 "Spade")
                                 (make-card 4 "Spade")
                                 (make-card 9 "Spade")
                                 (make-card 9 "Heart"))
                                10
                                "1")
              (make-GS
               (make-Split-State (list (make-card 10 "Club")
                                       (make-card 10 "Heart"))
                                 10
                                 (list (make-card 5 "Spade")
                                       (make-card 4 "Spade")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))

(check-expect (state-transition "start"
                                (list (make-card 5 "Heart")
                                      (make-card 5 "Club")
                                      (make-card 6 "Heart")
                                      (make-card 7 "Club")
                                      (make-card 9 "Spade")
                                      (make-card 9 "Heart"))
                                10
                                "1")
              (make-GS
               (make-Split-Double-State (list (make-card 5 "Heart")
                                              (make-card 5 "Club"))
                                        10
                                        (list (make-card 6 "Heart")
                                              (make-card 7 "Club")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))

(check-expect (state-transition "start"
                                (list (make-card 5 "Heart")
                                      (make-card 5 "Club")
                                      (make-card 6 "Heart")
                                      (make-card 7 "Club")
                                      (make-card 9 "Spade")
                                      (make-card 9 "Heart"))
                                10
                                "5")
              (make-GS
               (make-Split-Double-State (list (make-card 5 "Heart")
                                              (make-card 5 "Club"))
                                        5
                                        (list (make-card 6 "Heart")
                                              (make-card 7 "Club")))
               (list
                (make-card 9 "Spade")
                (make-card 9 "Heart"))
               10))


;;; choose to split: hit on first hand, dont bust => base-state
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1
                                                 (list
                                                  (make-card 4 "Spade"))) 
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
                                        1
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 10 "Diamond")
                        (make-card 10 "Heart"))
                       10))

;;; split => hit again on first split => not bust
(check-expect (state-transition (make-Base-State #f
                                                 (list
                                                  (make-card 6 "Diamond")
                                                  (make-card 2 "Club"))
                                                 (list
                                                  (make-card 6 "Heart"))
                                                 1
                                                 1
                                                 (list
                                                  (make-card 4 "Spade")))
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
                                        1
                                        (list
                                         (make-card 4 "Spade")))
                       (list
                        (make-card 10 "Heart"))
                       10))

;; split => hold/win on first split, hit on second split => no bust =>
; base state
;
;(check-expect (state-transition (make-Base-State (list
;                                                  (make-card 6 "Diamond")
;                                                  (make-card 2 "Club")
;                                                  (make-card 10 "Diamond"))
;                                                 (list
;                                                  (make-card 6 "Heart"))
;                                                 #f
;                                                 0
;                                                 1)
;                                (list
;                                 (make-card 10 "Heart"))
;                                12
;                                "h")
;              (make-GS (make-Base-State (list
;                                         (make-card 6 "Diamond")
;                                         (make-card 2 "Club")
;                                         (make-card 10 "Diamond"))
;                                        (list
;                                         (make-card 10 "Heart")
;                                         (make-card 6 "Heart"))
;                                        #f
;                                        0
;                                        1)
;                       '()
;                       12))
;
;; split => win on first split, hold on second split => no bust
;(check-expect (state-transition (make-Base-State (list
;                                                  (make-card 6 "Diamond")
;                                                  (make-card 2 "Club")
;                                                  (make-card 10 "Diamond"))
;                                                 (list
;                                                  (make-card 6 "Heart")
;                                                  (make-card 10 "Heart"))
;                                                 #f
;                                                 0
;                                                 1)
;                                '()
;                                12
;                                " ")
;              (make-GS (make-Round-Over-State 
;                        (list
;                         (make-card 6 "Diamond")
;                         (make-card 2 "Club")
;                         (make-card 10 "Diamond")
;                         (make-card 6 "Heart")
;                         (make-card 10 "Heart")))      
;                       '()
;                       12))
;
;
;
;
;;; choose to split: hit on first hand, dont bust => base-state
;(check-expect (state-transition (make-Base-State #f
;                                                 (list
;                                                  (make-card 6 "Diamond"))
;                                                 (list
;                                                  (make-card 6 "Heart"))
;                                                 1
;                                                 1) 
;                                (list
;                                 (make-card 2 "Club")
;                                 (make-card 8 "Spade")
;                                 (make-card 10 "Diamond")
;                                 (make-card 10 "Heart"))
;                                10
;                                "h")
;              (make-GS (make-Base-State #f
;                                        (list
;                                         (make-card 2 "Club")
;                                         (make-card 6 "Diamond"))
;                                        (list
;                                         (make-card 6 "Heart"))
;                                        1
;                                        1)
;                       (list
;                        (make-card 8 "Spade")
;                        (make-card 10 "Diamond")
;                        (make-card 10 "Heart"))
;                       10))
;
;;; split => hit again on first split => not bust => base state
;(check-expect (state-transition (make-Base-State #f
;                                                 (list
;                                                  (make-card 6 "Diamond")
;                                                  (make-card 2 "Club"))
;                                                 (list
;                                                  (make-card 6 "Heart"))
;                                                 1
;                                                 1)
;                                (list
;                                 (make-card 8 "Spade")
;                                 (make-card 10 "Diamond")
;                                 (make-card 10 "Heart"))
;                                10
;                                "h")
;              (make-GS (make-Base-State #f
;                                        (list
;                                         (make-card 8 "Spade")
;                                         (make-card 6 "Diamond")
;                                         (make-card 2 "Club"))
;                                        (list
;                                         (make-card 6 "Heart"))
;                                        1
;                                        1)
;                       (list
;                        (make-card 10 "Diamond")
;                        (make-card 10 "Heart"))
;                       10))
;
;
;;; split => lost on first split, hit on second split => no bust => base state
;;; note : should bet = 0?
;(check-expect (state-transition (make-Base-State 
;                                 (list
;                                  (make-card 6 "Diamond")
;                                  (make-card 2 "Club")
;                                  (make-card 8 "Spade"))
;                                 (list
;                                  (make-card 6 "Heart"))
;                                 #f
;                                 0
;                                 1)
;                                (list
;                                 (make-card 10 "Heart")
;                                 (make-card 10 "Diamond"))
;                                9
;                                "h")
;              (make-GS (make-Base-State (list
;                                         (make-card 6 "Diamond")
;                                         (make-card 2 "Club")
;                                         (make-card 8 "Spade"))
;                                        (list
;                                         (make-card 10 "Heart")
;                                         (make-card 6 "Heart"))
;                                        #f
;                                        0
;                                        1)
;                       (list
;                        (make-card 10 "Diamond"))
;                       9))
;
;;; split => lost first split, hit on second split => bust => round over state
;;(check-expect (state-transition (make-Base-State (list
;;                                                  (make-card 6 "Diamond")
;;                                                  (make-card 2 "Club")
;;                                                  (make-card 8 "Spade"))
;;                                                 (list
;;                                                  (make-card 6 "Heart")
;;                                                  (make-card 10 "Diamond"))
;;                                                 #f
;;                                                 0
;;                                                 1)
;;                                (list
;;                                 (make-card 10 "Heart"))
;;                                9
;;                                "h")
;;              (make-GS (make-Round-Over-State (list
;;                                               (make-card 6 "Diamond")
;;                                               (make-card 2 "Club")
;;                                               (make-card 8 "Spade")
;;                                               (make-card 10 "Heart")
;;                                               (make-card 6 "Heart")
;;                                               (make-card 10 "Diamond")
;;                                               ))
;;                       '()
;;                       8))
;
;

;; key : GS key -> GS
;; takes in GS and key and returns GS
;; Strategy: structural decomposition
(define (key GS key)
  (cond
    [(string=? "h" key)
     (gs-transition GS key)]
    [(string=? " " key)
     (gs-transition GS key)]
    [(string=? "s" key)
     (gs-transition GS key)]
    [(string=? "d" key)
     (gs-transition GS key)]
    [(string=? "5" key)
     (gs-transition GS key)]
    [(string=? "1" key)
     (gs-transition GS key)]))

(check-expect (key (make-GS "start"
                            (list (make-card 2 "Club")
                                  (make-card 3 "Club")
                                  (make-card 5 "Diamond")
                                  (make-card 6 "Diamond"))
                            100)
                   "5")
              (gs-transition (make-GS "start"
                                      (list (make-card 2 "Club")
                                            (make-card 3 "Club")
                                            (make-card 5 "Diamond")
                                            (make-card 6 "Diamond"))
                                      100)
                             "5"))

(check-expect (key (make-GS "start"
                            (list (make-card 2 "Club")
                                  (make-card 3 "Club")
                                  (make-card 5 "Diamond")
                                  (make-card 6 "Diamond"))
                            100)
                   "1")
              (gs-transition (make-GS "start"
                                      (list (make-card 2 "Club")
                                            (make-card 3 "Club")
                                            (make-card 5 "Diamond")
                                            (make-card 6 "Diamond"))
                                      100)
                             "1"))
                            

(check-expect (key (make-GS
                    (make-Double-State (list (make-card 4 "Spade")
                                             (make-card 5 "Spade"))
                                       1
                                       (list (make-card 5 "Heart")
                                             (make-card 6 "Diamond")))
                    (list (make-card 2 "Club")
                          (make-card 3 "Club"))
                    100)
                   "d")
              (gs-transition (make-GS
                              (make-Double-State (list (make-card 4 "Spade")
                                                       (make-card 5 "Spade"))
                                                 1
                                                 (list (make-card 5 "Heart")
                                                      (make-card 6 "Diamond")))
                              (list (make-card 2 "Club")
                                    (make-card 3 "Club"))
                              100)
                             "d"))

(check-expect (key (make-GS
                    (make-Split-State (list (make-card 4 "Spade")
                                            (make-card 4 "Spade"))
                                      1
                                      (list (make-card 5 "Heart")
                                            (make-card 6 "Diamond")))
                    (list (make-card 2 "Club")
                          (make-card 3 "Club"))
                    100)
                   "s")
              (gs-transition (make-GS
                              (make-Split-State (list (make-card 4 "Spade")
                                                      (make-card 4 "Spade"))
                                                1
                                                (list (make-card 5 "Heart")
                                                      (make-card 6 "Diamond")))
                              (list (make-card 2 "Club")
                                    (make-card 3 "Club"))
                              100)
                             "s"))

(check-expect (key (make-GS
                    (make-Base-State #f
                                     (list (make-card 4 "Spade")
                                           (make-card 3 "Club"))
                                     #f
                                     1
                                     #f
                                     (list (make-card 5 "Heart")
                                           (make-card 6 "Diamond")))
                    (list (make-card 2 "Club")
                          (make-card 5 "Club"))
                    100)
                   "h")
              (gs-transition (make-GS
                              (make-Base-State #f
                                               (list (make-card 4 "Spade")
                                                     (make-card 3 "Club"))
                                               #f
                                               1
                                               #f
                                               (list (make-card 5 "Heart")
                                                     (make-card 6 "Diamond")))
                              (list (make-card 2 "Club")
                                    (make-card 5 "Club"))
                              100)
                             "h"))

(check-expect (key (make-GS
                    (make-Base-State #f
                                     (list (make-card 4 "Spade")
                                           (make-card 3 "Club"))
                                     #f
                                     1
                                     #f
                                     (list (make-card 5 "Heart")
                                           (make-card 6 "Diamond")))
                    (list (make-card 2 "Club")
                          (make-card 5 "Club"))
                    100)
                   " ")
              (gs-transition (make-GS
                              (make-Base-State #f
                                               (list (make-card 4 "Spade")
                                                     (make-card 3 "Club"))
                                               #f
                                               1
                                               #f
                                               (list (make-card 5 "Heart")
                                                     (make-card 6 "Diamond")))
                              (list (make-card 2 "Club")
                                    (make-card 5 "Club"))
                              100)
                             " "))


(big-bang start-blackjack
  [to-draw   draw]
  [on-key    key])
  
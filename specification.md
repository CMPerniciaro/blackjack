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

UI Specifications:
- cards will have a flipping animation
- pre-determined bet amounts on the screen 
- keyboard controls to select bets, choose moves, etc.
- display wallet amount
- win and lose message/animation

1. Player is given starting "wallet" amount for betting
2. Deck(s) are shuffled
3. Player makes initial bet
4. Player dealt 2 cards face up
5. Dealer dealt two cards, one face up, one face down
6. Having Blackjack right away:
   - 6a. Player has 21 right away:
       - (6a.1) The dealer does not have 21 so the player will receive 1.5x the
              original bet from dealer
       - (6a.2) The dealer also has 21 so the Player gets their original bet back
   - 6b. Dealer has 21 right away:
       - (6b.1) The player does not have 21 right away so the dealer wins and
              takes the bets
       - (6b.2) See (6a.2)
7. The Player can Hit or Hold
   - 7a. Hit 
        - (7a.1) Bust
              - (7a.4) If player's wallet is less than or equal to 0, the game
                     is over and the player loses 
       - (7a.2) Get closer to 21
       - (7a.3) The player wins and collects their original bet x2
  - 7b. Hold
       - (7b.1) Player wins because they are the closest to 21 and collects x2
              their original bet 
       - (7b.2) Lose because dealer was closer
              - (7b.3) If player's wallet is less than or equal to 0, the game
                     is over and the player loses 
8. The Dealer can Hit or Hold
   - 8a. Dealer will hit until card total 17+
       - (8a.1) Ace = 11 when it can total their card to 17+
   - 8b. Dealer will hold when cards total 17+
9. Player can Split pairs
   - 9a. Split (treat cards as two separate hands)
       - (9a1) Original bet will be doubled and split across the two cards
       - (9a2) Card1 will be played normally
             - (9a2.1) See 7
       - (9a3) Card2 will be played normally
             - (9a3.1) See 7
   - 9b. Don't Split pairs
       - (9b.1) Play normally (See 7)
10. Player can Double Down
    - 10a. Double down
       - (10a1) Player doubles amount of original bet and is dealt one more card
       - (10a2) Player then plays normally (See 7)
    - 10b. Don't double down
       - (10b.1) Play normally (See 7)

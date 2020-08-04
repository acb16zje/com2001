# COM2001 Dominoes Game Assignment

| Assignment 1    | Credit | Mark    | Feedback                                                                                     |
|-----------------|--------|---------|----------------------------------------------------------------------------------------------|
| Data Structures | 15     | 15      |                                                                                              |
| goesP           | 10     | 10      |                                                                                              |
| knockingP       | 9      | 10      | Can also be defined in terms of possPlays by checking if list is empty                       |
| playedP         | 10     | 10      |                                                                                              |
| possPlays       | 14     | 15      | Long functions like possPlaysA can be split into smaller re-usable ones for left/right sides |
| playDom         | 9      | 10      |                                                                                              |
| scoreBoard      | 15     | 15      |                                                                                              |
| scoreN          | 15     | 15      |                                                                                              |
| **Total**       | **97** | **100** |                                                                                              |

**Comments**: Good testing and good documentation

| Assignment 2    | Credit | Mark    | Feedback                                                                                                                                                                                                 |
|-----------------|--------|---------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Design          | 18     | 20      | Very clear and easy to follow design. Well documented. Core components seen: (playDomsRound, shuffleDoms, DomsPlayers - "naivePlayer" (or simplePlayer) & "hsdPlayer")                                   |
| Data Structures | 13     | 15      | Data structure okay. You should have imported Assignment1 code and not clutter this one.  Did not implement a type of "gameState" that keeps track of board, players' hands, score and whose turn it is. |
| Doms Players    | 14     | 15      | "naivePlayer" (or simplePlayer) vs "hsdPlayer" Implemented two players as instructed                                                                                                                     |
| shuffleDoms     | 14     | 15      | Works okay                                                                                                                                                                                               |
| playDomsRound   | 33     | 35      | playDomsRound implemented as defined.                                                                                                                                                                    |
| **Total**       | **92** | **100** |                                                                                                                                                                                                          |

**Comments**: Exhaustively tested and well reported.

| Assignment 3                                                                                                                                                                                                                                                                                       | Credit                                                   | Mark                                                     |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|----------------------------------------------------------|
| **Design**                                                                                                                                                                                                                                                                                         |                                                          |                                                          |
| <ul> <li>Does the design provide a framework for implementing a number of different players?</li> <li>Are the player themselves well-designed: is it clear from the design how the players decide on their moves?</li> <ul>                                                                        | <ul> <li>10</li> <li>8</li> </ul>                        | <ul> <li>10</li> <li>10</li> </ul>                       |
| **Implementation**                                                                                                                                                                                                                                                                                 |                                                          |                                                          |
| <ul> <li>Is the framework well-coded, making good use of Haskell features?</li> <li>Is the framework well-tested?</li> <li>Are the players well-coded?</li> <li>Are the players well-tested?</li> </ul>                                                                                            | <ul> <li>13</li> <li>4</li> <li>18</li> <li>5</li> </ul> | <ul> <li>15</li> <li>5</li> <li>25</li> <li>5</li> </ul> |
| **Results**                                                                                                                                                                                                                                                                                        |                                                          |                                                          |
| <ul> <li>Are there comparative results for the players?</li> <li>Are the results well-presented?</li> <li>Do the results show how performance improves as more knowledge is added to the player?</li> <li>How smart are the players? How much knowledge about the game do they capture?</li> </ul> | <ul> <li>5</li> <li>5</li> <li>5</li> <li>13</li> </ul>  | <ul> <li>5</li> <li>5</li> <li>5</li> <li>15</li> </ul>  |
| **Total**                                                                                                                                                                                                                                                                                          | **86**                                                   | **100**                                                  |

**Comments**: 
  - Excellent piece of work
  - Very good framework. Good design doc.
  - Clever tactics.
  - Odd that you try all tactics rather than recursing till one triggers
  - Some code in Tactics could be better .. if then elses, nested lets

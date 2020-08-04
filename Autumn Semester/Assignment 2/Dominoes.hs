-- Dominoes.hs, a dominoes game by ZerJun Eng

module Dominoes where
  import Data.List
  import Debug.Trace
  import System.Random

  -- datatype for domino
  -- Use tuple of two Ints to represent the value of a domino
  type Domino = (Int, Int)

  -- datatype for hand
  -- A hand is a list of dominoes
  type Hand = [Domino]

  -- datatype for board
  -- A board is a list of dominoes, but it must be like [(_, a), (a, _)]
  type Board = [Domino]

  -- datatype for end
  -- An end is either the Left or Right end of a board
  data End = L | R deriving (Eq, Show, Ord)

  -- datatype for DomsPlayer
  -- Takes a Hand and a Board and returns a Domino to play and the End to play it at
  type DomsPlayer = Hand -> Board -> (Domino, End)

  -- datatype for Turn
  -- Use to specify a turn for Player 1 or Player 2
  data Turn = P1 | P2 deriving (Eq, Show)

  -- datatype for Hands
  -- A pair of hands to represent the hand for Player 1 and Player 2
  type Hands = (Hand, Hand)

  -- datatype for Scores
  -- Use tuple of two Ints to represent the score of Player 1 and Player 2
  type Scores = (Int, Int)

  -- domSet: Full dominoes set
  domSet = [(a,b) | a <- [0..6], b <- [0..a]] :: [Domino]

  {- VARIABLE NAMES
      dom, (a,b) : A domino
      hand       : A hand
      hands      : A pair of hands for Player 1 and Player 2
      brd        : A board
      end        : An end
      p,r        : left pip or left dom
      q,s        : right pip or right dom
      p1         : Player 1
      p2         : Player 2
      p1Hand     : Player 1 hand
      p2Hand     : Player 2 hand
      rp1Hand    : Player 1 hand after making a move
      rp2Hand    : Player 2 hand after making a move
      np1Score   : Player 1 score after making move
      np2Score   : Player 2 score after making a move
  -}

  -- simplePlayer: plays the first domino in its hand which will go
  simplePlayer :: DomsPlayer
  simplePlayer (dom:rhand) brd
      | goesP dom brd L = (dom, L)               -- plays the first domino to left if it will go
      | goesP dom brd R = (dom, R)               -- plays the first domino to right if it will go
      | otherwise       = simplePlayer rhand brd

  -- hsdPlayer: plays the highest scoring domino in its hand
  hsdPlayer :: DomsPlayer
  hsdPlayer hand brd = (dom, end) -- Return the highest scoring domino
    where
      hands         = possPlays hand brd
      (_, dom, end) = maxHsd hands brd

  -- maxHsd: compares and returns the highest scoring domino between hsdLeft and hsdRight
  maxHsd :: Hands -> Board -> (Int, Domino, End)
  maxHsd (playL, playR) brd
      | null playL = hsdRight playR brd                           -- No possible plays at left, return the highest scoring domino at right
      | null playR = hsdLeft playL brd                            -- No possible plays at right, return the highest scoring domino at left
      | otherwise  = max (hsdLeft playL brd) (hsdRight playR brd) -- Compares and return the highest scoring domino

  -- hsdLeft: finds the maximum scoring domino played at left
  hsdLeft :: Hand -> Board -> (Int, Domino, End)
  hsdLeft playL brd = maximum [(tryPlay dom brd L, dom, L) | dom <- playL]  -- finds the domino which gives maximum score by playing on left

  -- hsdRight: finds the maximum scoring domino played at right
  hsdRight :: Hand -> Board -> (Int, Domino, End)
  hsdRight playR brd = maximum [(tryPlay dom brd R, dom, R) | dom <- playR] -- finds the domino which gives maximum score by playing on right

  -- shuffleDoms: returns a list of all the dominoes in a random order
  shuffleDoms :: Int -> [Domino]
  shuffleDoms seed = [dom | (_, dom) <- sortList]           -- returns the randomised dominoes list
    where
      rngList  = take 28 (randoms (mkStdGen seed) :: [Int]) -- create a list of 28 randomised integers
      zipList  = zip rngList domSet                         -- zip them with dominoes set, eg. (-123, (0,0))
      sortList = sort zipList                               -- sort them according to the randomised integers

  -- createHands: takes a list of randomised dominoes and returns a pair of hands
  createHands :: [Domino] -> Hands
  createHands domList = (p1Hand, p2Hand)
    where
      p1Hand = take 9 domList            -- take the first 9 randomised dominoes
      p2Hand = take 9 (drop 9 domList)   -- take the next 9 by dropping the first 9 dominoes

  -- playDomsRound: return the score of each player after playing a full round
  --                return type is a pair, eg. (8, 6)
  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> Scores
  playDomsRound p1 p2 seed = startTurn p1 p2 hands [] (0,0) P1 -- start a full round, and return the final scores of each player
    where
      hands = createHands (shuffleDoms seed)                   -- create a pair of hands for Player 1 and Player 2

  -- startTurn: start the turn of Player 1 or Player 2
  --            if a player is knocking, then start the turn of next player
  --            return type is a tuple, which represent the score of two players respectively
  startTurn :: DomsPlayer -> DomsPlayer -> Hands -> Board -> Scores -> Turn -> Scores
  startTurn p1 p2 hands@(p1Hand, p2Hand) brd (p1Score, p2Score) turn
      | endGame hands brd        = (p1Score, p2Score)                                            -- Base case, the game is ended when both players are knocking
      | p1CanPlay turn hands brd = startTurn p1 p2 (rp1Hand, p2Hand) brd1 (np1Score, p2Score) P2 -- If Player 1 can play, start the turn of Player 2 after Player 1 has played
      | p2CanPlay turn hands brd = startTurn p1 p2 (p1Hand, rp2Hand) brd2 (p1Score, np2Score) P1 -- If Player 2 can play, start the turn of Player 1 after Player 2 has played
    where
      (rp1Hand, brd1, score1) = playerPlay p1 p1Hand brd                                         -- Player 1 makes a move
      (rp2Hand, brd2, score2) = playerPlay p2 p2Hand brd                                         -- Player 2 makes a move
      np1Score                = p1Score + score1                                                 -- Add the turn score of Player 1 to his total score
      np2Score                = p2Score + score2                                                 -- Add the turn score of Player 2 to his total score

  -- endGame: check whether there are any possible plays for both player
  --          True when both player are knocking
  endGame :: Hands -> Board -> Bool
  endGame (p1Hand, p2Hand) brd = knockingP p1Hand brd && knockingP p2Hand brd

  -- p1CanPlay: check whether Player 1 can play in this turn
  --            True when it is P1 turn and Player 1 is not knocking; or Player 2 is knocking
  p1CanPlay :: Turn -> Hands -> Board -> Bool
  p1CanPlay turn (p1Hand, p2Hand) brd = (turn == P1) && not (knockingP p1Hand brd) || knockingP p2Hand brd

  -- p2CanPlay: check whether Player 2 can play in this turn
  --            True when it is P2 turn and Player 2 is not knocking; or Player 1 is knocking
  p2CanPlay :: Turn -> Hands -> Board -> Bool
  p2CanPlay turn (p1Hand, p2Hand) brd = (turn == P2) && not (knockingP p2Hand brd) || knockingP p1Hand brd

  -- playerPlay: A Player makes a move
  --             return a triplet of the Player's new hand, resulting board, and the Player's score this turn
  playerPlay :: DomsPlayer -> Hand -> Board -> (Hand, Board, Int)
  playerPlay player hand brd = (rhand, nbrd, score)
    where
      (dom, end) = player hand brd               -- Return the domino and the end the Player should play at
      nbrd       = resMaybe(playDom dom brd end) -- The resulting board after playing the domino
      rhand      = delete dom hand               -- Remove the played domino from the Player's hand
      score      = scoreBoard nbrd               -- Calculate the score after playing the domino

  -- findLast: find the last domino on the board
  -- Base case, only 1 domino in the board, answer is that domino
  -- Otherwise, keep calling itself recursively until only 1 domino is left
  findLast :: Board -> Domino
  findLast [h]   = h
  findLast (_:t) = findLast t

  -- resMaybe: extract the result from playDom, helper function
  resMaybe :: Maybe Board -> Board
  resMaybe (Just brd) = brd

  -- goesP: return True if a domino can be played
  goesP :: Domino -> Board -> End -> Bool
  goesP (a,b) brd end
      | null brd                         = True  -- Base case, true if board is empty
      | (end == L) && (a == p || b == p) = True  -- Given end is L, True if the first or second value of the domino match the first value of the first domino in the board
      | (end == R) && (a == q || b == q) = True  -- Given end is R, True if the first or second value of the domino match the second value of the last domino in the board
      | otherwise                        = False
    where
      (p, _):_ = brd
      (_, q)   = findLast brd

  -- knockingP: return True if no domino can be played on the board
  -- Uses possPlays - true if that finds nothing to go either L or R
  knockingP :: Hand -> Board -> Bool
  knockingP hand brd = null playL && null playR
    where
      (playL, playR) = possPlays hand brd

  -- playedP: return True if a domino has already been played
  playedP :: Domino -> Board -> Bool
  playedP dom brd = not (any (sameDomP dom) brd) -- A domino is played if there is a same domino in the board

  -- sameDomP: Are 2 dominoes the same? Allowing for reverse order
  sameDomP :: Domino -> Domino -> Bool
  sameDomP (p,q) (r,s)
      | p == r = q == s   -- True if (p,q) = (r,s)
      | p == s = q == r   -- True if (p,q) = (s,r)
      | otherwise = False

  -- possPlays: return all playable dominoes on L and R on given hand
  --            return type is a pair, ([*playable on L*], [*playable on R*])
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays [] _ = ([], [])                                        -- No possible plays if hand is empty
  possPlays hand brd
      | null brd  = (hand, hand)                                   -- Base case, all dominoes in the hand can be played if board
      | otherwise = (possPlaysA hand brd L, possPlaysA hand brd R) -- Calls its auxiliary function to get the possible plays for left and right end

  -- possPlaysA: auxiliary function for possPlays
  possPlaysA :: Hand -> Board -> End -> [Domino]
  possPlaysA hand brd end = [dom | dom <- hand, goesP dom brd end] -- Return the list of dominoes that can go to the given end

  -- playDom: return a new Board if a given domino can be played
  playDom :: Domino -> Board -> End -> Maybe Board
  playDom dom brd end
      | null brd  = Just (dom:brd)           -- Base case, board is empty, domino can be played directly
      | playL     = Just (playLeft dom brd)  -- Can be played at left
      | playR     = Just (playRight dom brd) -- Can be played at right
      | otherwise = Nothing                  -- Return Nothing if the given domino cannot be played at the given end
    where
      playL = end == L && goesP dom brd L
      playR = end == R && goesP dom brd R

  -- playLeft: play to left if it will go
  playLeft :: Domino -> Board -> Board
  playLeft (a,b) brd
      | a == p    = (b,a):brd -- Swap the domino
      | otherwise = (a,b):brd
    where
      (p, _):_ = brd

  -- playLeft: play to right if it will go
  playRight :: Domino -> Board -> Board
  playRight (a,b) brd
      | a == q    = brd ++ [(a,b)]
      | otherwise = brd ++ [(b,a)] -- Swap the domino
    where
      (_, q) = findLast brd

  -- scoreBoard: take a return a 5s-and-3s score
  scoreBoard :: Board -> Int
  scoreBoard brd
      | null brd        = 0                     -- Base case, 0 score if the board is empty
      | mod sum 15 == 0 = div sum 5 + div sum 3 -- If the total can be divided by 15, calculate the 5s and 3s
      | mod sum 5 == 0  = div sum 5             -- If the total can be divided by 5 only, calculate the 5s
      | mod sum 3 == 0  = div sum 3             -- If the total can be divided by 3 only, calculate the 3s
      | otherwise       = 0
    where
      sum = sumEnds brd                         -- Calls the sumEnds function to calculate the total

  -- sumEnds: sum and return the total ends on the board
  sumEnds :: Board -> Int
  sumEnds brd
      | (p,q) == (r,s)   = p + q       -- If there is only 1 domino on the board, then add the first and second value only
      | p == q && r == s = (p + s) * 2 -- Two double dominoes at both end
      | p == q           = p * 2 + s   -- A double domino at left end
      | r == s           = s * 2 + p   -- A double domino at right end
      | otherwise        = p + s       -- Otherwise just add the first value of the first domino and the second value of the last domino
    where
      (p, q):_ = brd
      (r, s)   = findLast brd

  -- scoreN: return all dominoes in pair that will give a score of n
  --         ([dominoes that give score n when played at L], [dominoes that give score n when played at R])
  scoreN :: Board -> Int -> ([Domino], [Domino])
  scoreN brd n = (scoreNA playL brd n L, scoreNA playR brd n R) -- Calls its auxiliary function to check if a domino gives score n
    where
      boneYrd = [dom | dom <- domSet, playedP dom brd]          -- create list of unplayed dominoes
      (playL, playR) = possPlays boneYrd brd                    -- playL is the possible dominoes to be played at left

  -- scoreNA: auxiliary function for scoreN, add dominoes that give score n into the list
  --          considering all playable dominoes are "hand"
  scoreNA :: Hand -> Board -> Int -> End -> [Domino]
  scoreNA (dom:rhand) brd n end
      | null brd                 = []                          -- Base case, return [] if hand is empty
      | tryPlay dom brd end == n = dom:scoreNA rhand brd n end -- If the domino gives n score after playing it, then add the domino into the list
      | otherwise                = scoreNA rhand brd n end     -- Otherwise, calls itself again with the rest of the hand

  -- tryPlay: try playing the domino at the given end on the board and return the score
  tryPlay :: Domino -> Board -> End -> Int
  tryPlay dom brd end = scoreBoard (resMaybe(playDom dom brd end))
  
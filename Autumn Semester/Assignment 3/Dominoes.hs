-- dominoes.hs, a dominoes game by ZerJun Eng

module Dominoes where
  import Data.List
  import Data.Maybe
  import Debug.Trace
  import System.Random

  -- Use tuple of two Ints to represent the value of a domino
  type Domino = (Int, Int)

  -- A hand is a list of dominoes
  type Hand = [Domino]

  -- InitBoard is initial state; In Board, the two Domino are the most left and right dominoes
  data DomBoard = InitBoard | Board Domino Domino History deriving (Show)

  -- An end is either the Left or Right end of a board
  data End = L | R deriving (Eq, Show, Ord)

  -- Use to specify a turn for Player 1 or Player 2
  data Player = P1 | P2 deriving (Eq, Show)

  -- Use tuple of two Ints to represent the score of Player 1 and Player 2
  type Scores = (Int, Int)

  -- Takes a Hand and a Board and returns a Domino to play and the End to play it at
  type DomsPlayer = Hand -> DomBoard -> Player -> Scores -> (Domino, End)

  -- History allows course of game to be reconstructed
  type History = [(Domino, Player, MoveNum)]

  -- Specify the move order
  type MoveNum = Int

  -- state in a game - p1's hand, p2's hand, player to drop, current board, scores
  type GameState = (Hand, Hand, Player, DomBoard, Scores)

  -- tactic for skillPlayer
  type Tactic = Hand -> DomBoard -> Player -> Scores -> Maybe (Domino, End)

  -- domSet: Full dominoes set
  domSet = [(a,b) | a <- [0..6], b <- [0..a]] :: [Domino]

  {- VARIABLE NAMES
      dom, (d1,d2) : A domino
      hand         : A hand
      brd          : A board
      end          : An end
      (l1,l2)      : most left dom
      (r1,r2)      : most right dom
      p1           : Player 1
      p2           : Player 2
      h1           : Player 1 hand
      h2           : Player 2 hand
      ns1          : Player 1 score after making move
      ns2          : Player 2 score after making a move
  -}

  -----------------------------------------------------------------------------------------
  -- Tactics framework, can easily add / remove or change the order
  tactics :: [Tactic]
  tactics = [firstDropTactic,
             aggressiveTactic,
             comboToWinTactic,
             matchPointTactic,
             luckyWinTactic,
             blockWinTactic,
             drawGameTactic]

  -- skillPlayer: plays the domino based on several tactics
  skillPlayer :: [Tactic] -> DomsPlayer
  skillPlayer tactics hand brd player scores
      | null tacticRes = hsdPlayer hand brd player scores -- play highest scoring domino if all tactics fail
      | otherwise      = head tacticRes                   -- otherwise, play the first tactic that return a result
    where
      tacticRes = [fromJust play | tactic <- tactics,
                   let play = tactic hand brd player scores, isJust play]

  -----------------------------------------------------------------------------------------
  -- Early game Tactics

  -- firstDropTactic: Play (5,4) if it is first drop and (5,4) in hand and it is start of a game
  --                  otherwise try to play the domino that will score to 61
  firstDropTactic :: Tactic
  firstDropTactic hand InitBoard player (s1, s2)
      | s1 /= 0           = playWinningDom InitBoard (possPlays hand InitBoard) score -- same as matchPointTactic, extremely useful
      | (5,4) `elem` hand = Just ((5,4), L)                                           -- most popular first drop
      | otherwise         = Nothing
    where
      score = if player == P1 then s1 else s2

  -- Ignore if it is not an empty board
  firstDropTactic _ brd _ _ = Nothing

  -----------------------------------------------------------------------------------------
  -- Late game Tactics

  -- aggressiveTactic: extremely powerful against player who doesn't have many tactics and have
  --                   predictable hand
  -- If the predicted hand is accurate, then most of the time it can give a win
  aggressiveTactic :: Tactic
  aggressiveTactic _ InitBoard _ _ = Nothing -- this case has been covered by firstDropTactic

  -- Case 1: If the opponent score is less than 53, which means it's still not late game yet
  -- Case 2: If the player score is more than 52, and got one of the highest scoring domino which will not bust
  -- Case 3: If the player got dominoes which will give to score 59
  aggressiveTactic hand brd@(Board _ _ hist) player s@(s1, s2)
      | oppScore < 53 || null maxDom         = Nothing
      | pScore > 52 && not (null maxDomSafe) = Just (snd (maximumBy (comparing fst) maxDomSafe)) -- play the domino in the list which gives highest score
      | not (null maxDom59)                  = Just (head maxDom59)                              -- play the first domino which gives to 59
      | otherwise                            = Just (snd (maximumBy (comparing fst) maxDom))     -- play the highest scoring domino (hope for the best)
    where
      opp                = if player == P1 then P2 else P1
      (pScore, oppScore) = if player == P1 then (s1, s2) else (s2, s1)
      (playL, playR)     = possPlays hand brd
      oppHand            = predictOpponentHand hist player hand
      maxDomL            = [(nscore, (dom, L))| dom <- playL,
                            let nscore = scoreDom dom L brd,
                            let nbrd = updateBoard dom L player brd,
                            isNothing (playWinningDom nbrd (possPlays oppHand nbrd) oppScore)]
      maxDomR            = [(nscore, (dom, R))| dom <- playR,
                            let nscore = scoreDom dom R brd,
                            let nbrd = updateBoard dom R player brd,
                            isNothing (playWinningDom nbrd (possPlays oppHand nbrd) oppScore)]
      maxDom             = maxDomL ++ maxDomR                                 -- list of dominoes, and the score they will give
      maxDomSafe         = [(s, dom) | (s, dom) <- maxDom, (s + pScore) < 62] -- Filter out those dominoes which will bust
      maxDom59           = [dom | (s, dom) <- maxDom, (s + pScore) == 59]     -- Filter out those dominoes which will not give to score 59

  -- comboToWinTactic: knock the opponent, then play the winning domino
  -- Find the domino that will knock the opponent in the next round,
  -- and we will have a winning domino after that
  -- not really useful and very rare case
  comboToWinTactic :: Tactic
  comboToWinTactic _ InitBoard _ _ = Nothing -- this case has been covered by firstDropTactic

  comboToWinTactic hand brd player (s1, s2)
      | null knockToWinDom = Nothing
      | otherwise          = Just (head knockToWinDom)
    where
      score         = if player == P1 then s1 else s2
      knockOppDom   = knockOpponentDom hand brd player                   -- list of dominoes that can knock the opponent
      knockToWinDom = [(dom, end) | (dom, end) <- knockOppDom,
                       let tmpBrd = updateBoard dom end player brd,
                       let tmpHand = possPlays (delete dom hand) tmpBrd, -- list of dominoes that can knock the opponent, and
                       let tmpScore = scoreDom dom end brd + score,      -- there will be a winning domino after that move
                       isJust (playWinningDom tmpBrd tmpHand tmpScore)]

  -- matchPointTactic: play the domino which score exactly to 61
  -- extremely powerful and common case in dominoes game
  matchPointTactic :: Tactic
  matchPointTactic _ InitBoard _ _ = Nothing -- this case is covered by firstDropTactic

  matchPointTactic hand brd player (s1, s2)
      | isJust winningDom  = winningDom
      | otherwise = Nothing
    where
      score      = if player == P1 then s1 else s2
      winningDom = playWinningDom brd (possPlays hand brd) score -- the domino which will win directly

  -- luckyWinTactic: just play the domino which leads us to the matchPointTactic
  luckyWinTactic :: Tactic

  -- Scores should not be (0,0) in here, so we should play logically
  -- this case has been covered by firstDropTactic
  luckyWinTactic _ InitBoard _ _ = Nothing

  -- This state is when s1 or s2 is close to 61, and we hope the lucky domino
  -- will knock or opponent doesn't win in next round
  luckyWinTactic hand brd player (s1, s2)
      | null luckyList = Nothing
      | otherwise      = Just luckyDom
    where
      score          = if player == P1 then s1 else s2
      (playL, playR) = possPlays hand brd
      luckyWinL      = [(domScore, (dom, L)) | dom <- playL,
                        let tmpBrd = playLeft player dom brd,
                        let tmpHand = possPlays (delete dom hand) tmpBrd,
                        let domScore = scoreDom dom L brd,
                        let tmpScore = scoreDom dom L brd + score,
                        isJust (playWinningDom tmpBrd tmpHand tmpScore)]  -- possible lucky dominoes on left
      luckyWinR      = [(domScore, (dom, R)) | dom <- playR,
                        let tmpBrd = playRight player dom brd,
                        let tmpHand = possPlays (delete dom hand) tmpBrd,
                        let domScore = scoreDom dom R brd,
                        let tmpScore = domScore + score,
                        isJust (playWinningDom tmpBrd tmpHand tmpScore)]  -- possible lucky dominoes on right
      luckyList      = luckyWinL ++ luckyWinR
      (_, luckyDom)  = maximumBy (comparing fst) luckyList                -- get the lucky domino that will give the highest score

  -- blockWinTactic: predict the opponent hand, and block him from playing the direct winning domino
  -- If the opponent can win the next round after I play a domino, then try to block him by playing another domino
  -- the predicted opponent hand must be very accurate for it to be very useful
  blockWinTactic :: Tactic
  blockWinTactic _ InitBoard _ _ = Nothing -- this case has been covered by firstDropTactic

  blockWinTactic hand brd@(Board _ _ hist) player (s1, s2)
      | null knockList       = Nothing
      | isJust oppWinningDom = Just (head knockList)
      | otherwise            = Nothing
    where
      (playL, playR) = possPlays hand brd
      oppScore       = if player == P1 then s2 else s1
      oppHand        = predictOpponentHand hist player hand                -- predict the opponent hand
      oppWinningDom  = playWinningDom brd (possPlays oppHand brd) oppScore -- see if the opponent has a winning dom
      knockList      = knockOpponentDom hand brd player

  -- drawGameTactic: play the domino that will knock the opponent, and try to draw the game
  -- the predicted opponent hand must be very accurate for it to be very useful
  drawGameTactic :: Tactic
  drawGameTactic _ InitBoard _ _ = Nothing -- this case has been covered by firstDropTactic

  drawGameTactic hand brd@(Board _ _ hist) player (s1, s2)
      | willBust && not(null knockList) = Just (head knockList)                     -- knock the opponent if the going player is confirmed to bust
      | otherwise                       = Nothing
    where
      score          = if player == P1 then s1 else s2
      (playL, playR) = possPlays hand brd
      willBust       = minimum ([scoreDom dom L brd | dom <- playL] ++
                                [scoreDom dom R brd | dom <- playR]) > (61 - score) -- minimum score the player can get is over 61
      knockList      = knockOpponentDom hand brd player                             -- find the knocking domino

  -----------------------------------------------------------------------------------------
  -- Tactics helper function

  -- knockOpponentDom: find the list of domino that will knock the opponent, based on his previous knocking hint
  knockOpponentDom :: Hand -> DomBoard -> Player -> [(Domino, End)]
  knockOpponentDom hand brd@(Board _ _ hist) player = knockList
    where
      (playL, playR) = possPlays hand brd
      oppHand        = predictOpponentHand hist player hand                                -- predict the opponent hand
      knockingL      = [(dom, L) | dom <- playL, knockingP oppHand (playLeft P1 dom brd)]  -- dominoes that knock opponent when play on left
      knockingR      = [(dom, R) | dom <- playR, knockingP oppHand (playRight P1 dom brd)] -- dominoes that knock opponent when play on right
      knockList      = knockingL ++ knockingR

  -- playWinningDom: play the winning domino that gives to exactly 61 score
  playWinningDom :: DomBoard -> (Hand, Hand) -> Int -> Maybe (Domino, End)
  playWinningDom brd (playL, playR) score
      | null winningDom = Nothing
      | otherwise       = Just (head winningDom)                                    -- choose the first one (since all dominoes can win)
    where
      winningDom = [(dom, L) | dom <- playL, scoreDom dom L brd == (61 - score)] ++ -- find all dominoes that can win directly
                   [(dom, R) | dom <- playR, scoreDom dom R brd == (61 - score)]

  -- predictOpponentHand: predict the opponent's hand based on what he is knocking on, and what he didn't play
  -- hist  : the history
  -- player: the player who wishes to know the opponent's hand
  -- hand  : the player hand
  predictOpponentHand :: History -> Player -> Hand -> Hand
  predictOpponentHand hist player hand = finalHand
    where
      opp       = if player == P1 then P2 else P1
      brd       = reconstructDomBoard hist
      weakList  = opponentWeakTiles hist player                                                               -- opponent weak pip values
      pDom      = [dom | (dom, p, _) <- hist, p == player] ++ hand                                            -- player's dom
      boneYrd   = [(a,b) | (a,b) <- domSet, (a,b) `notElem` pDom, (b,a) `notElem` pDom]                       -- sleeping dominoes
      initHand  = [(a,b) | (a,b) <- boneYrd, a `notElem` weakList, b `notElem` weakList]                      -- initial predicting hand
      prevBrd   = [reconstructFromMove hist (moveNum - 1) | (dom, p, moveNum) <- hist, moveNum > 1, p == opp] -- list of history board before opponent played
      exptPlay  = [dom | (Board l r h) <- prevBrd,                                                            -- the expected play by opponent
                   let rBrd = reconstructDomBoard h,
                   let oppHand = [(a,b) | (a,b) <- initHand, (a,b) `notElem` rBrd, (b,a) `notElem` rBrd],
                   let (dom, _) = hsdPlayer oppHand (Board l r h) opp (0, 0)]
      finalHand = [(a,b) | (a,b) <- initHand, (a,b) `notElem` brd, (b,a) `notElem` brd] \\ exptPlay           -- if the opponent doesn't play the expected play,
                                                                                                              -- exclude from his hand

  -- opponentWeakTiles: Get the list of values that the opponent is knocking on
  -- hist  : the history
  -- player: the player who wishes to know the opponent weak tiles
  opponentWeakTiles :: History -> Player -> [Int]
  opponentWeakTiles [] _ = []
  opponentWeakTiles hist player
      | null opponentKnockingOn = []
      | otherwise               = nub (opponentWeakTilesOnMove hist opponentKnockingOn)      -- remove duplicate pip values
    where
      sortedHist         = sortBy (\ (_,_,m1) (_,_,m2) -> (compare m1 m2)) hist
      playerHist         = [moveNum | (_, p, moveNum) <- sortedHist, p == player]            -- the history of the player
      opponentKnockingOn = [moveNum | moveNum <- playerHist, succ moveNum `elem` playerHist] -- opponent knocks on a move number when player plays two turn consecutively

  -- opponentWeakTilesOnMove: recursive function, return all the pip values for opponentWeakTiles
  opponentWeakTilesOnMove :: History -> [MoveNum] -> [Int]
  opponentWeakTilesOnMove hist moveNum
      | null moveNum = []                                    -- base case, recursion stop
      | otherwise    = l1:r2:opponentWeakTilesOnMove hist t  -- add the pip vale where the opponent is knocking
    where
      h:t     = moveNum
      oldHist = [dom | (dom, _, score) <- hist , score <= h] -- the history board on the move number
      (l1, _) = head oldHist
      (_, r2) = last oldHist

  -- reconstructDomBoard: reconstruct the whole board from the history
  reconstructDomBoard :: History -> [Domino]
  reconstructDomBoard hist = [dom | (dom, _, _) <- hist]

  -- reconstructFromMove: reconstruct the board from history on a certain move
  reconstructFromMove :: History -> MoveNum -> DomBoard
  reconstructFromMove hist moveNum = Board l r oldHist
    where
      oldHist = [(dom, p, m) | (dom, p, m) <- hist, m <= moveNum]
      (l, _, _) = head oldHist
      (r, _, _) = last oldHist





  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  -- simplePlayer: plays the first domino in its hand which will go
  simplePlayer :: DomsPlayer
  simplePlayer hand brd _ _
      | not (null playL) = (head playL, L)
      | otherwise        = (head playR, R)
    where
      (playL, playR) = possPlays hand brd

  ------------------------------------------------------------------------------------------
  -- hsdPlayer plays highest scoring dom
  hsdPlayer :: DomsPlayer

  hsdPlayer hand InitBoard _ _  = (maxDom, L)
    where
      domScores   = [(score53 (d1 + d2), (d1,d2)) | (d1,d2) <- hand]
      (_, maxDom) = maximumBy (comparing fst) domScores

  hsdPlayer hand brd _ _  = if scoreL > scoreR then (domL, L) else (domR, R)
    where
      (playL, playR) = possPlays hand brd
      scoresL        = [(scoreDom dom L brd, dom) | dom <- playL] -- [(domino, score)]
      scoresR        = [(scoreDom dom R brd, dom) | dom <- playR]
      (scoreL, domL) = if null scoresL then (-1, (0,0)) else maximumBy (comparing fst) scoresL
      (scoreR, domR) = if null scoresR then (-1, (0,0)) else maximumBy (comparing fst) scoresR

  -- need comparing
  -- useful fn specifying what we want to compare by
  comparing :: Ord b => (a -> b) -> a -> a -> Ordering
  comparing f l r = compare (f l) (f r)

  -----------------------------------------------------------------------------------------
  {- top level fn
      args: 2 players (p1, p2), number of games (n), random number seed (seed)
      returns: number of games won by player 1 & player 2
      calls playDomsGames giving it n, initial score in games and random no gen
  -}

  domsMatch :: DomsPlayer -> DomsPlayer -> Int -> Int -> (Int, Int)
  domsMatch p1 p2 n seed = playDomsGames p1 p2 n (0, 0) (mkStdGen seed)

  -----------------------------------------------------------------------------------------
  {- playDomsGames plays n games

  p1,p2 players
  (s1,s2) their scores
  gen random generator
  -}

  playDomsGames :: DomsPlayer -> DomsPlayer -> Int -> Scores -> StdGen -> Scores
  playDomsGames _ _ 0 score_in_games _ = score_in_games -- stop when n games played

  playDomsGames p1 p2 n (s1,s2) gen
      | gameRes == P1 = playDomsGames p1 p2 (n-1) (s1+1, s2) gen2 -- p1 won
      | otherwise     = playDomsGames p1 p2 (n-1) (s1, s2+1) gen2 -- p2 won
    where
      (gen1, gen2) = split gen -- get 2 generators, so doms can be reshuffled for next hand
      gameRes      = playDomsGame p1 p2 (if odd n then P1 else P2) (0,0) gen1 -- play next game p1 drops if n odd else p2

  -----------------------------------------------------------------------------------------
  -- playDomsGame plays a single game - 61 up
  -- returns winner - P1 or P2
  -- the Bool pdrop is true if it's p1 to drop
  -- pdrop alternates between games
  playDomsGame :: DomsPlayer -> DomsPlayer -> Player -> Scores -> StdGen -> Player
  playDomsGame p1 p2 pdrop scores gen
      | s1 == 61  = P1
      | s2 == 61  = P2
      | otherwise = playDomsGame p1 p2 (if pdrop == P1 then P2 else P1) (s1,s2) gen2
    where
      (gen1, gen2) = split gen
      (s1, s2)     = playDomsHand p1 p2 pdrop scores gen1

  -----------------------------------------------------------------------------------------
  -- play a single hand
  playDomsHand :: DomsPlayer -> DomsPlayer -> Player -> Scores -> StdGen -> Scores
  playDomsHand p1 p2 nextPlayer scores gen = playDoms p1 p2 init_gamestate
    where
      spack          = shuffleDoms gen
      p1_hand        = take 9 spack
      p2_hand        = take 9 (drop 9 spack)
      init_gamestate = (p1_hand, p2_hand, nextPlayer, InitBoard, scores)

  ------------------------------------------------------------------------------------------
  -- shuffleDoms: returns a list of all the dominoes in a random order
  shuffleDoms :: StdGen -> [Domino]
  shuffleDoms seed = [dom | (_, dom) <- sortList] -- returns the randomised dominoes list
    where
      rngList  = take 28 (randoms seed :: [Int])  -- create a list of 28 randomised integers
      zipList  = zip rngList domSet               -- zip them with dominoes set, eg. (-123, (0,0))
      sortList = sort zipList                     -- sort them according to the randomised integers

  ------------------------------------------------------------------------------------------
  -- playDoms runs the hand
  -- returns scores at the end
  playDoms :: DomsPlayer -> DomsPlayer -> GameState -> Scores
  playDoms _ _ (_,_,_,_, (61,s2)) = (61,s2) --p1 has won the game
  playDoms _ _ (_,_,_,_, (s1,61)) = (s1,61) --p2 has won the game

  playDoms p1 p2 gs@(h1, h2, nextPlayer, b, scores)
      | kp1 && kp2                  = scores                        -- both players knocking, end of the hand
      | nextPlayer == P1 && not kp1 = playDoms p1 p2 (p1Play p1 gs) -- p1 plays, returning new gameState. p2 to go next
      | nextPlayer == P1            = playDoms p1 p2 (p2Play p2 gs) -- p1 knocking so p2 plays
      | not kp2                     = playDoms p1 p2 (p2Play p2 gs) -- p2 plays
      | otherwise                   = playDoms p1 p2 (p1Play p1 gs) -- p2 knocking so p1 plays
    where
      kp1 = knockingP h1 b -- true if p1 knocking
      kp2 = knockingP h2 b -- true if p2 knocking

  ------------------------------------------------------------------------------------------
  -- player p1 to drop
  p1Play :: DomsPlayer -> GameState -> GameState

  p1Play p1 (h1, h2, _, brd, (s1,s2)) =
      (delete dom h1, h2, P2, updateBoard dom end P1 brd, (ns1, s2))
    where
      (dom, end) = p1 h1 brd P1 (s1, s2)            -- call the player, returning dom dropped and end it's dropped at.
      score      = s1 + scoreDom dom end brd        -- what it scored
      ns1        = if score > 61 then s1 else score -- check for going bust


  -- player p2 to drop
  p2Play :: DomsPlayer -> GameState -> GameState

  p2Play p2 (h1, h2, _, brd, (s1,s2)) =
      (h1, delete dom h2, P1, updateBoard dom end P2 brd, (s1, ns2))
    where
      (dom, end) = p2 h2 brd P2 (s1,s2)             -- call the player, returning dom dropped and end it's dropped at.
      score      = s2 + scoreDom dom end brd        -- what it scored
      ns2        = if score > 61 then s2 else score -- check for going bust

  -- update the board after a play
  updateBoard :: Domino -> End -> Player -> DomBoard -> DomBoard
  updateBoard dom end player brd
    | end == L  = playLeft player dom brd
    | otherwise = playRight player dom brd

  ------------------------------------------------------------------------------------------
  -- will given domino go at left?
  goesLP :: Domino -> DomBoard -> Bool
  goesLP _ InitBoard = True
  goesLP (d1,d2) (Board (l,_) _ _) = (l == d1) || (l == d2)

  -- will dom go to the right?
  goesRP :: Domino -> DomBoard -> Bool
  goesRP _ InitBoard = True
  goesRP (d1,d2) (Board _ (_,r) _) = (r == d1) || (r == d2)

  ------------------------------------------------------------------------------------------
  -- knockingP: return True if no domino can be played on the board
  -- Uses possPlays - true if that finds nothing to go either L or R
  knockingP :: Hand -> DomBoard -> Bool
  knockingP hand brd = null playL && null playR
    where
      (playL, playR) = possPlays hand brd

  ------------------------------------------------------------------------------------------
  -- possPlays: return all playable dominoes on L and R on given hand
  --            return type is a pair, ([*playable on L*], [*playable on R*])
  possPlays :: Hand -> DomBoard -> (Hand, Hand)
  possPlays [] _           = ([], [])       -- No possible plays if hand is empty
  possPlays hand InitBoard = (hand, hand)   -- Base case, all dominoes in the hand can be played if board
  possPlays hand brd       = (playL, playR)
    where
      playL = [dom | dom <- hand, goesLP dom brd]
      playR = [dom | dom <- hand, goesRP dom brd]

  ------------------------------------------------------------------------------------------
  -- playDom
  -- given player plays
  -- play a dom at left or right, if it will go
  playDom :: Player -> Domino -> End -> DomBoard -> Maybe DomBoard

  playDom player dom L brd
      | goesLP dom brd = Just (playLeft player dom brd)
      | otherwise      = Nothing

  playDom player dom R brd
      | goesRP dom brd = Just (playRight player dom brd)
      | otherwise      = Nothing

  -- play to left - it will go
  playLeft :: Player -> Domino -> DomBoard -> DomBoard

  playLeft player dom InitBoard = Board dom dom [(dom, player, 1)]

  playLeft player (d1,d2) (Board (l1,l2) r hist)
      | d1 == l1  = Board (d2,d1) r (((d2,d1), player, n):hist)
      | otherwise = Board (d1,d2) r (((d1,d2), player, n):hist)
    where
      n = maximum [m | (_, _, m) <- hist] + 1 -- next drop number

  -- play to right
  playRight :: Player -> Domino -> DomBoard -> DomBoard

  playRight player dom InitBoard = Board dom dom [(dom, player, 1)]

  playRight player (d1,d2) (Board l (r1,r2) hist)
      | d1 == r2  = Board l (d1,d2) (hist ++ [((d1,d2), player, n)])
      | otherwise = Board l (d2,d1) (hist ++ [((d2,d1), player, n)])
    where
      n = maximum [m | (_, _, m) <- hist] + 1 -- next drop number

  ------------------------------------------------------------------------------------------
  -- scoreDom
  -- what will a given Dom score at a given end?
  -- assuming it goes

  scoreDom :: Domino -> End -> DomBoard -> Int
  scoreDom dom end brd = scoreboard nb
    where
      (Just nb) = playDom P1 dom end brd -- player doesn't matter

  ------------------------------------------------------------------------------------------
  -- scoreBoard: take a return a 5s-and-3s score
  scoreBoard :: DomBoard -> Int

  scoreBoard InitBoard = 0

  scoreboard (Board (l1, l2) (r1, r2) hist)
      | length hist == 1 = score53 (l1 + l2) -- 1 dom played, it's both left and right end
      | otherwise        = score53 ((if l1 == l2 then 2*l1 else l1) + (if r1 == r2 then 2*r2 else r2))

  score53 :: Int -> Int
  score53 n = s3 + s5
    where
      s3 = if rem n 3 == 0 then quot n 3 else 0
      s5 = if rem n 5 == 0 then quot n 5 else 0

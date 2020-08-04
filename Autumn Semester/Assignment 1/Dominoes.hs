-- Dominoes.hs, a dominoes game by ZerJun Eng

module Dominoes where
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
  data End = L | R deriving (Eq)

  -- domR: dominoes range, 0 to 6
  domR = [0..6] :: [Int]

  -- findLast: find the last domino on the board
  -- Base case, only 1 domino in the board, answer is that domino
  -- Otherwise, keep calling itself recursively until only 1 domino is left
  findLast :: Board -> Domino
  findLast [h]   = h
  findLast (_:t) = findLast t

  -- append: add the element the last of the list
  -- Base case, board is empty, domino can be added directly
  -- Otherwise, it calls itself recursively until the board is empty(base case)
  append :: Board -> Domino -> Board
  append [] dom       = [dom]
  append (h:rbrd) dom = h:append rbrd dom

  -- resMaybe: extract the result from playDom, helper function
  resMaybe :: Maybe Board -> Board
  resMaybe (Just brd) = brd

  -- goesP: return True if a domino can be played
  goesP :: Domino -> Board -> End -> Bool
  goesP dom brd end
      | null brd                         = True -- Base case, true if board is empty
      | (end == L) && (a == p || b == p) = True -- Given end is L, True if the first or second value of the domino match the first value of the head domino in the board
      | (end == R) && (a == q || b == q) = True -- Given end is R, True if the first or second value of the domino match the second value of the last domino in the board
      | otherwise = False
    where
      (a, b)     = dom
      ((p, _):_) = brd
      (_, q)     = findLast brd

  -- knockingP: return True if no domino can be played on the board
  knockingP :: Hand -> Board -> Bool
  knockingP hand brd
      | null hand                          = True                -- Base case, True if the hand is empty
      | null brd                           = False               -- Base case, False if the board is empty
      | goesP dom brd L || goesP dom brd R = False               -- False if the first domino in the hand can be played on the Left or Right
      | otherwise                          = knockingP rhand brd -- Otherwise calls itself again with the rest of the hand
    where
      dom:rhand = hand

  -- playedP: return True if a domino has already been played
  playedP :: Domino -> Board -> Bool
  playedP dom brd
      | null brd                       = False            -- Base case, board is empty, nothing is played yet
      | dom == (p, q) || dom == (q, p) = True             -- True if the given domino is same as the first domino in the board
      | otherwise                      = playedP dom rbrd -- Calls itself again with the rest of the board
    where
      (p, q):rbrd = brd

  -- possPlays: return all playable dominoes on L and R on given hand
  --            return type is a pair, ([*playable on L*], [*playable on R*])
  possPlays :: Hand -> Board -> ([Domino], [Domino])
  possPlays hand brd
      | null brd || null hand = (hand, hand)                   -- Base case, all dominoes in the hand can be played if board or hand is empty
      | otherwise             = (possPlaysA hand brd lasDom L,
                                  possPlaysA hand brd lasDom R) -- Calls its auxiliary function to get the possible plays for left and right end
    where
      lasDom = findLast brd

  -- possPlayA: auxiliary function for possPlays
  possPlaysA :: Hand -> Board -> Domino -> End -> [Domino]
  possPlaysA hand brd lasDom end
      | null hand         = []                                   -- Base case, return [] if hand is empty
      | playL && (b == p) = dom:possPlaysA rhand brd lasDom L    -- Add to the left playable list if its goesP return True and (a, b),(p, _) where b = p
      | playR && (a == q) = dom:possPlaysA rhand brd lasDom R    -- Add to the right playable list if its goesP return True and (_, q),(a, b) where a = q
      | playL             = (b, a):possPlaysA rhand brd lasDom L -- Same as above but the domino is flipped before adding
      | playR             = (b, a):possPlaysA rhand brd lasDom R
      | otherwise         = possPlaysA rhand brd lasDom end      -- Calls itself again until reaching the base case
    where
      dom:rhand  = hand
      (a, b)     = dom
      ((p, _):_) = brd
      (_, q)     = lasDom
      playL      = end == L && goesP dom brd L
      playR      = end == R && goesP dom brd R

  -- playDom: return a new Board if a given domino can be played
  playDom :: Domino -> Board -> End -> Maybe Board
  playDom dom brd end
      | null brd          = Just (dom:brd)           -- Base case, board is empty, domino can be played directly
      | playL && (b == p) = Just (dom:brd)           -- Add to the head of the board if the domino is played at the left and its goesP return True
      | playR && (a == q) = Just (append brd dom)    -- Add to the last of the board if the domino is played at the right and its goesP return True
      | playL             = Just ((b, a):brd)        -- Same as above but the domino is flipped to become [(b, a),(p, _),...]
      | playR             = Just (append brd (b, a)) -- Same as above but the domino is flipped to become [...,(_, q),(b, a)]
      | otherwise         = Nothing                  -- Return Nothing if the given domino cannot be played at the given end
    where
      (a, b)     = dom
      ((p, _):_) = brd
      (_, q)     = findLast brd
      playL      = end == L && goesP dom brd L
      playR      = end == R && goesP dom brd R

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
      | (p, q) == (r, s) = p + q       -- If there is only 1 domino on the board, then add the first and second value only
      | p == q && r == s = (p + s) * 2 -- Two double dominoes at both end
      | p == q           = p * 2 + s   -- A double domino at left end
      | r == s           = s * 2 + p   -- A double domino at right end
      | otherwise        = p + s       -- Otherwise just add the first value of the first domino and the second value of the last domino
    where
      ((p, q):_) = brd
      (r, s)     = findLast brd

  -- scoreN: return all dominoes in pair that will give a score of n
  --         ([dominoes that give score n when played at L], [dominoes that give score n when played at R])
  scoreN :: Board -> Int -> ([Domino], [Domino])
  scoreN brd n = (scoreNA playL brd n L, scoreNA playR brd n R) -- Calls its auxiliary function to check if a domino gives score n
    where
      (playL, playR) = possPlays (boneYrd domR domR brd) brd    -- playL is the possible dominoes to be played at left

  -- scoreNA: auxiliary function for scoreN, add dominoes that give score n into the list
  --          considering all playable dominoes are "hand"
  scoreNA :: Hand -> Board -> Int -> End -> [Domino]
  scoreNA hand brd n end
      | null hand                = []                          -- Base case, return [] if hand is empty
      | scoreBoard tryPlay == n  = dom:scoreNA rhand brd n end -- If the domino gives n score after playing it, then add the domino into the list
      | otherwise                = scoreNA rhand brd n end     -- Otherwise, calls itself again with the rest of the hand
    where
      dom:rhand = hand
      tryPlay   = resMaybe(playDom dom brd end)                -- Try playing the domino at the given end on the board

  -- boneYrd: create all dominoes that have not been played and
  --          can be played on either end
  -- It starts by (0,0),(0,1)...(0,6) then (1,1)...(1,6),(2,2)...(6,6), a total of 28 dominoes
  boneYrd :: [Int] -> [Int] -> Board -> [Domino]
  boneYrd lis1 lis2 brd
      | null lis1            = []                           -- The first list is finished and the function is ended, i.e it has finished until (6,6),
      | null lis2            = boneYrd t1 t1 brd            -- The second list is finished, e.g. it has finished until (0,6), next is (1,1)
      | playedP (h1, h2) brd = boneYrd lis1 t2 brd          -- If the domino (h1,h2) is played, then calls itself with the new lis2 as the tail of the current lis2
      | matchingDom          = (h1, h2):boneYrd lis1 t2 brd -- If the domino (h1,h2) can be played on L or R, then add it into the list
      | otherwise            = boneYrd lis1 t2 brd          -- Otherwise just calls itself again, same as the case where the domino is played
    where
      h1:t1       = lis1
      h2:t2       = lis2
      matchingDom = goesP (h1, h2) brd L || goesP (h1, h2) brd R
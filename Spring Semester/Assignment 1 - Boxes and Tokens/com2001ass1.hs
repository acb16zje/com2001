-- AMENDED VERSION OF SUBMITTED FILE
module TestFile_154 where
  import Test.Tasty
  import Test.Tasty.HUnit
  --
  -- START OF SUBMITTED CODE
  --
  {-
       COM2001 Spring Assignment 1
       Haskell Template
       (c) 2018 Mike Stannett
       Email: m.stannett@sheffield.ac.uk
  -}

  -- If you would like to add your name and/or registration number
  -- to this file, please do so here:
  --
  -- Name: ZerJun Eng
  -- Registration Number: 160203853

  {-  Testing methods included
        - Boundary case, e.g Box 0 or maximum Box
        - Random box number, initialised by user input
        - Random box number, not initialised by user input, e.g Box 999, but user only provided [1,2,3]
  -}

  type Input  = Int
  type Output = Int

  -- A program is something that tells a computer how to
  -- move from one configuration to another, how to
  -- recognize when a configuration represents a valid
  -- accept state, and so on.

  class (Eq cfg) => ProgrammableComputer cfg where
    initialise   :: Program -> [Input] -> cfg
    getOutput    :: cfg -> Output
    acceptState  :: Program -> cfg -> Bool
    doNextMove   :: Program -> cfg -> cfg
    runFrom      :: Program -> cfg -> cfg
    runProgram   :: Program -> [Input] -> cfg
    -- Default implementation
    runProgram p is = runFrom p (initialise p is)


  -- The BATcomputer has just 3 types of instruction
  -- CLR b        == empty box b
  -- INC b        == add a token to box b
  -- JEQ b1 b2 t  == if boxes b1 and b2 contain the same
  --                 number of tokens, jump to instruction t
  --
  data Instruction
    = CLR {box :: Int}
    | INC {box :: Int}
    | JEQ {box1   :: Int,
           box2   :: Int,
           target :: Int}
    deriving (Eq, Show)

  type Program = [Instruction]


  -- PROBLEM 1. YOUR CODE HERE
  -- --------------------------
  -- Each instruction in a program refers to one or
  -- more boxes.  What is the highest box number used
  -- anywhere in the program?

  {-  Testing
        1. maxBoxNum []
          - Expected: 0
          - Result: 0
          - Status: Pass
        2. maxBoxNum [CLR 0]
          - Expected: 0
          - Result: 0
          - Status: Pass
        3. maxBoxNum [INC 5]
          - Expected: 5
          - Result: 5
          - Status: Pass
        4. maxBoxNum [JEQ 6 3 2]
          - Expected: 6
          - Result: 6
          - Status: Pass
        5. maxBoxNum [CLR 1, INC 2, JEQ 8 4 1]
          - Expected: 8
          - Result: 8
          - Status: Pass
        6. maxBoxNum [CLR 1, INC 345, JEQ 80 0 1]
          - Expected: 345
          - Result 345
          - Status: Pass
  -}
  maxBoxNum :: Program -> Int
  maxBoxNum pg
      | null pg                                   = 1
      | max_clr < 0 || max_inc < 0 || max_jeq < 0 = 1
      | otherwise                                 = max (max max_clr max_inc) max_jeq
    where
      max_clr = if null clr_list then 0 else maximum clr_list -- Get the maximum box in CLR instruction
      max_inc = if null inc_list then 0 else maximum inc_list -- Get the maximum box in INC instruction
      max_jeq = if null jeq_list then 0 else maximum jeq_list -- Get the maximum box in JEQ instruction
      clr_list = [n | CLR n <- pg]
      inc_list = [n | INC n <- pg]
      jeq_list = [max n1 n2 | JEQ n1 n2 _ <- pg]


  -- The configuration of a BATcomputer is given once
  -- you know how many tokens are in each box, and
  -- which instruction should be executed next
  data BATConfig = BATConfig {
      boxes   :: [Int],
      counter :: Int
      } deriving (Eq)


  -- PROBLEM 2. YOUR CODE HERE
  -- --------------------------
  {-  Testing
        1. test = BATConfig [] 0
          - Expected: boxes = []; counter = 0
          - Result: boxes = []; counter = 0
          - Status: Pass
        2. test = BATConfig [7,8,9] 0
          - Expected: boxes = [7,8,9]; counter = 0
          - Result: boxes = [7,8,9]; counter = 0
          - Status: Pass
        3. test = BATConfig [0,233] 0
          - Expected: boxes = [0,233]; counter = 0
          - Result: boxes = [0,233]; counter = 0
          - Status: Pass
  -}
  instance Show BATConfig where
      show x = "boxes = " ++ show (boxes x) ++ "; counter = " ++ show (counter x)


  -- IMPLEMENTING THE BATComputer
  -- ============================
  -- User inputs run from Box 1 onwards. Output is what ends up in Box 1.
  -- Box 0 can be used by programs for calculations.
  instance ProgrammableComputer BATConfig  where
      -- PROBLEM 3: initialise   :: Program -> [Input] -> cfg
      -- initialise: Add Box 0 to input and set counter to 0

      {-  Testing
            Unable to test it individually, testing is included with execute function
      -}
      initialise pg input
          | maxBoxNum pg > length input = BATConfig (0:input ++ boxesToAdd) 0  -- create additional boxes if user didn't supply enough boxes to match the maximum box number
          | otherwise                   = BATConfig (0:input) 0
        where
          boxesToAdd = [0 | a <- [1..(maxBoxNum pg - length input)]]           -- maximum number of boxes in program - length of user input = boxes to add


      -- PROBLEM 4: acceptState  :: Program -> cfg -> Bool
      -- acceptState: Program is finished when the counter exceeds or equal to the program length

      {-  Testing
            1. acceptState [] (BATConfig [] 0)
              - Expected: True
              - Result: True
              - Status: Pass
            2. acceptState [] (BATConfig [1,2,3] 0)
              - Expected: True
              - Result: True
              - Status: Pass
            3. acceptState [CLR 3, INC 2] (BATConfig [] 0)
              - Expected: False
              - Result: False
              - Status: Pass
            4. acceptState [CLR 3, INC 2] (BATConfig [1,2,3] 0)
              - Expected: False
              - Result: False
              - Status: Pass
            5. acceptState [CLR 999] (BATConfig [1,2,3] 1)
              - Expected: True
              - Result: True
              - Status: Pass
      -}
      acceptState pg (BATConfig boxes ct)
          | null pg || ct < 0  = True
          | otherwise          = ct >= length pg


      -- PROBLEM 5: doNextMove   :: Program -> cfg -> cfg
      -- doNextMove: Possible next moves are CLR, INC and JEQ.

      {-  Testing
           ** In here, Box 0 is actually Box 1. If it is run thorough 'execute', initialise will add a Box 0 automatically
            1. doNextMove [] (BATConfig [] 0)
              - Expected: boxes = []; counter = 0
              - Result: boxes = []; counter = 0
              - Status: Pass
            2. doNextMove [] (BATConfig [1,2,3] 0)
              - Expected: boxes = [1,2,3]; counter = 0
              - Result: boxes = [1,2,3]; counter = 0
              - Status: Pass
            3. doNextMove [INC 0] (BATConfig [] 0)
              - Expected: boxes = []; counter = 0
              - Result: boxes = []; counter = 0
              - Status: Pass
            4. doNextMove [CLR 0] (BATConfig [1,2,3] 0)
              - Expected: boxes = [0,2,3]; counter = 1
              - Result: boxes = [0,2,3]; counter = 1
              - Status: Pass
            5. doNextMove [INC 0] (BATConfig [1,2,3] 0)
              - Expected: boxes = [2,2,3]; counter = 1
              - Result: boxes = [2,2,3]; counter = 1
              - Status: Pass
            6. doNextMove [JEQ 1 1 2] (BATConfig [1,2,3] 0)
              - Expected: boxes = [1,2,3]; counter = 2
              - Result: boxes = [1,2,3]; counter = 2
              - Status: Pass
      -}
      doNextMove pg cfg@(BATConfig boxes ct)
          | null pg            = cfg                  -- nothing left to process
          | null boxes         = cfg                  -- user input is empty
          | acceptState pg cfg = cfg                  -- already got the answer
          | otherwise          = BATConfig boxes' ct' -- update the boxes and counter based on current instruction
        where
          -- When running through 'execute', Box 0 is added into the input. Therefore 'take n boxes' will take all boxes before the specified box,
          -- 'drop (n+1) boxes' will remain all boxes after the specified box
          (boxes', ct') =
              case pg !! ct of
                  CLR n     -> (take n boxes ++ 0 : drop (n+1) boxes, ct + 1)                -- clear the specified box, increase the counter by 1
                  INC n     -> (take n boxes ++ (boxes !! n) + 1 : drop (n+1) boxes, ct + 1) -- increase the specified box, increase the counter by 1
                  JEQ x y n -> (boxes, if (boxes !! x) == (boxes !! y) then n else ct + 1)   -- if Bx == By then change the counter to n, else increase the counter by 1


      -- PROBLEM 6: runFrom      :: Program -> cfg -> cfg
      -- runFrom: keep running the program until acceptState is reached

      {-  Testing
           ** In here, Box 0 is actually Box 1. If it is run thorough 'execute', initialise will add a Box 0 automatically
            1. runFrom [] (BATConfig [] 0)
              - Expected: boxes = []; counter = 0
              - Result: boxes = []; counter = 0
              - Status: Pass
            2. runFrom [] (BATConfig [1,2,3] 0)
              - Expected: boxes = [1,2,3]; counter = 0
              - Result: boxes = [1,2,3]; counter = 0
              - Status: Pass
            3. runFrom [INC 7] (BATConfig [] 0)
              - Expected: boxes = []; counter = 0
              - Result: boxes = []; counter = 0
              - Status: Pass
            4. runFrom [CLR 0] (BATConfig [1,2,3] 0)
              - Expected: boxes = [0,2,3]; counter = 1
              - Result: boxes = [0,2,3]; counter = 1
              - Status: Pass
            5. runFrom [CLR 0] (BATConfig [1,2,3] 3)
              - Expected: boxes = [1,2,3]; counter = 3
              - Result: boxes = [1,2,3]; counter = 3
              - Status: Pass
            6. runFrom [JEQ 1 1 2, CLR 0, INC 1] (BATConfig [1,2,3] 0)
              - Expected: boxes = [1,3,3]; counter = 3
              - Result: boxes = [1,3,3]; counter = 3
              - Status: Pass
      -}
      runFrom pg cfg@(BATConfig boxes ct)
          | null pg            = cfg                            -- nothing left to process
          | null boxes         = cfg                            -- user input is empty
          | acceptState pg cfg = cfg                            -- already got the answer
          | otherwise          = runFrom pg (doNextMove pg cfg) -- make a move and try to run again


      -- PROBLEM 7: getOutput    :: cfg -> Output
      -- getOutput: return the value of Box 1

      {-  Testing
           ** In here, it will return the value of second index because Box 0 is added during initialisation
           ** Therefore, when running through 'execute', it will return the value of Box 1
            1. getOutput (BATConfig [] 0)
              - Expected: 0
              - Result: 0
              - Status: Pass
            2. getOutput (BATConfig [1] 0)
              - Expected: 0
              - Result: 0
              - Status: Pass
            3. getOutput (BATConfig [1,2,3,4,5] 0)
              - Expected: 2
              - Result: 2
              - Status: Pass
            4. getOutput (BATConfig [0,1,2,3,4,5] 0)
              - Expected: 1
              - Result: 1
              - Status: Pass
      -}
      getOutput cfg@(BATConfig boxes _)
          | null boxes       = 0           -- user input is empty
          | length boxes < 2 = 0           -- if the number of boxes is less 2, we can't get the value of Box 1 since the only box is Box 0
          | otherwise        = boxes !! 1  -- otherwise return the value of Box 1


  -- This function is included to help with testing. Running
  -- "execute p xs" should show the output generated when
  -- running program p with user input(s) xs

  {-  Testing
       ** Tested with adder, copyBox and addXY too
        1. execute [] []
          - Expected: 0
          - Result: 0
          - Status: Pass
        2. execute [] [1,2,3]
          - Expected: 1
          - Result: 1
          - Status: Pass
        3. execute [INC 2] []
          - Expected: 0
          - Result: 0
          - Status: Pass
        4. execute [JEQ 1 2 0] [1,2,3]
          - Expected: 1
          - Result: 1
          - Status: Pass
        5. execute [CLR 0, CLR 2, INC 1] [1,2,3]
          - Expected: 2
          - Result: 2
          - Status: Pass
        6. execute [INC 1, INC 1, INC 1] [1,2,3]
          - Expected: 4
          - Result: 4
          - Status: Pass
  -}
  execute :: Program -> [Input] -> Output
  execute p ins = getOutput (runProgram p ins :: BATConfig)


  -- PROBLEM 8. YOUR CODE HERE
  -- ---------------------------
  -- start a program at instruction n instead of 0.  In other
  -- words, change Jump instructions from (J x y t) to (J x y (t+n))
  -- and leave all other instructions unchanged.

  {-  Testing
        1. transpose 0 []
          - Expected: []
          - Result: []
          - Status: Pass
        2. transpose 9 []
          - Expected: []
          - Result: []
          - Status: Pass
        3. transpose 0 [JEQ 1 2 3, JEQ 4 5 0]
          - Expected: [JEQ {box1 = 1, box2 = 2, target = 3},JEQ {box1 = 4, box2 = 5, target = 0}]
          - Result: [JEQ {box1 = 1, box2 = 2, target = 3},JEQ {box1 = 4, box2 = 5, target = 0}]
          - Status: Pass
        4. transpose 3 [JEQ 1 2 3, JEQ 4 5 0]
          - Expected: [JEQ {box1 = 1, box2 = 2, target = 6},JEQ {box1 = 4, box2 = 5, target = 3}]
          - Result: [JEQ {box1 = 1, box2 = 2, target = 6},JEQ {box1 = 4, box2 = 5, target = 3}]
          - Status: Pass
        5. transpose (-2) [JEQ 1 2 3, JEQ 4 5 0]
          - Expected: [JEQ {box1 = 1, box2 = 2, target = 3},JEQ {box1 = 4, box2 = 5, target = 0}]
          - Result: [JEQ {box1 = 1, box2 = 2, target = 3},JEQ {box1 = 4, box2 = 5, target = 0}]
          - Status: Pass
  -}
  transpose :: Int -> Program -> Program
  transpose n pg
      | null pg   = pg                   -- if p is empty, then return an empty list
      | null t    = ins : t              -- nothing left to process, return the changed list
      | otherwise = ins : transpose n t  -- call itself recursively to change JEQ instruction
    where
      h:t = pg
      ins = case h of
              CLR x     -> CLR x         -- unchanged for CLR instruction
              INC x     -> INC x         -- unchanged for INC instruction
              JEQ x y t -> JEQ x y (t+n)


  -- PROBLEM 9. YOUR CODE HERE
  -- ---------------------------
  -- join two programs together, so as to run one
  -- after the other

  {-  Testing
        1. [] *->* []
          - Expected: []
          - Result: []
          - Status: Pass
        2. [CLR 0, INC 1] *->* []
          - Expected: [CLR {box = 0},INC {box = 1}]
          - Result: [CLR {box = 0},INC {box = 1}]
          - Status: Pass
        3. [] *->* [JEQ 2 2 4]
          - Expected: [JEQ {box1 = 2, box2 = 2, target = 4}]
          - Result: [JEQ {box1 = 2, box2 = 2, target = 4}]
          - Status: Pass
        4. [JEQ 1 2 3, INC 1, INC 1, INC 1] *->* [JEQ 2 2 4]
          - Expected: [JEQ {box1 = 1, box2 = 2, target = 3},INC {box = 1},INC {box = 1},INC {box = 1},JEQ {box1 = 2, box2 = 2, target = 8}]
          - Result: [JEQ {box1 = 1, box2 = 2, target = 3},INC {box = 1},INC {box = 1},INC {box = 1},JEQ {box1 = 2, box2 = 2, target = 8}]
          - Status: Pass
  -}
  (*->*) :: Program -> Program -> Program
  p1 *->* p2
      | null p1   = p2                              -- no transpose if p1 is empty
      | null p2   = p1                              -- no transpose if p2 is empty
      | otherwise = p1 ++ transpose (length p1) p2  -- the JEQ instruction of p2 must be transposed to match the added length of p1


  -- PROBLEM 10. YOUR CODE HERE
  -- ---------------------------
  -- program to compute B1 = B1 + B2
  {-
      0. clear Box 0
      1. if Box 0 and Box 2 are equal, jump to 5, else continue
      2. increase Box 0
      3. increase Box 1
      4. if Box 0 and Box 0 are equal, jump to 1, this is to continue the loop for adding
         values into Box 0 and Box 1 until Box 0 and Box 2 are equal, which means now B1 = B1 + B2
      5. clear Box 0
  -}

  {-  Testing
        1. execute adder [];
          - Expected: 0
          - Result: 0
          - Status: Pass
        2. execute adder [1];
          - Expected: 1
          - Result: 1
          - Status: Pass
        3. execute adder [1,2];
          - Expected: 3
          - Result: 3
          - Status: Pass
        4. execute adder [1,4,7];
          - Expected: 5
          - Result: 5
          - Status: Pass
  -}
  adder :: Program
  adder = [CLR 0, JEQ 0 2 5, INC 0, INC 1, JEQ 0 0 1, CLR 0]


  -- PROBLEM 11. YOUR CODE HERE
  -- ---------------------------
  -- create a program to copy the contents of box m to box n (leave box m unchanged)
  {-
      0. clear Box 0
      1. clear Box n
      2. if Box m and Box n have equal values (finished copying), jump to 5, else continue
      3. increase Box n
      4. if Box 0 and Box 0 are equal, jump to 2, this is to continue the loop for adding
         values into Box n until Box m and Box n are equal
      5. clear Box 0
  -}

  {-  Testing
       ** Will only display the value of Box 1 because getOutput only return the value of Box 1,
         but addXY works correctly and it uses copyBox, so it should work for all cases
        1. execute (copyBox 0 0) []
          - Expected: 0
          - Result: 0
          - Status: Pass
        2. execute (copyBox 0 0) [1,2,3,4,5]
          - Expected: 1
          - Result: 1
          - Status: Pass
        3. execute (copyBox 5 1) [1,2,3,4,5]
          - Expected: 5
          - Result: 5
          - Status: Pass
        4. execute (copyBox 20 1) [1,2,3,4,5]
          - Expected: 0
          - Result: 0
          - Status: Pass
  -}
  copyBox :: Int -> Int -> Program
  copyBox m n
      | m == n    = []  -- we don't need to copy a box to itself
      | otherwise = [CLR 0, CLR n, JEQ m n 5, INC n, JEQ 0 0 2]


  -- PROBLEM 12. YOUR CODE HERE
  -- ---------------------------
  -- program to compute B1 = Bx + By

  {-  Testing
        1. execute (addXY 1 2) []
          - Expected: 0
          - Result: 0
          - Status: Pass
        2. execute (addXY 0 0) [1,2,3,4,5]
          - Expected: 0
          - Result: 0
          - Status: Pass
        3.
          - Expected: 2
          - Result: 2
          - Status: Pass
        4. execute (addXY 1 5) [1,2,3,4,5]
          - Expected: 6
          - Result: 6
          - Status: Pass
        5. execute (addXY 3 0) [1,2,3,4,5]
          - Expected: 3
          - Result: 3
          - Status: Pass
        6. execute (addXY 100 2) [1,2,3,4,5]
          - Expected: 2
          - Result: 2
          - Status: Pass [CLR 0, JEQ 0 2 5, INC 0, INC 1, JEQ 0 0 1, CLR 0]
  -}
  addXY :: Int -> Int -> Program
  addXY 1 1 = [CLR 0, JEQ 0 1 6, INC 0, INC 0, INC 1, JEQ 0 0 1, CLR 1, JEQ 0 1 10, INC 1, JEQ 0 0 7, CLR 0]
  addXY x y = copyBox x 1 *->* [CLR 0, JEQ 0 y 5, INC 0, INC 1, JEQ 0 0 1, CLR 0]


  -- END OF TEMPLATE FILE
  -- END OF SUBMITTED CODE


  ------------------------------------
  ------------------------------------
  ------------------------------------
  ------------------------------------
  ------------------------------------


  -- START OF TEST CASES

  -- import Test.Tasty
  -- import Test.Tasty.HUnit


  -- set some arbitrary box, token-count and instruction numbers
  imax, bmax, tmax :: Int
  imax = 2651
  bmax = 1027
  tmax = 214

  -- Generate an arbitrary configuration
  cfg0 = initialise [CLR 5] [7] :: BATConfig


  -------------------------------------------------------------
  -- PROBLEM 1 : maxBoxNum
  -------------------------------------------------------------
  testset1 :: TestTree
  testset1 = testGroup "================ PROBLEM 1: maxBoxNum" [
    testset1_boundaries,
    testset1_general,
    testset1_errors
    ]

  testset1_boundaries = testGroup "==== Boundaries" [
    testCase "maxBoxNum []" $
      maxBoxNum [] @?= 1,
    testCase "maxBoxNum [CLR n]" $
      maxBoxNum [CLR bmax] @?= bmax,
    testCase "maxBoxNum [INC n]" $
      maxBoxNum [INC bmax] @?= bmax,
    testCase "maxBoxNum [JEQ n 20 30]" $
      maxBoxNum [JEQ bmax 20 30] @?= bmax,
    testCase "maxBoxNum [JEQ 10 n 30]" $
      maxBoxNum [JEQ 10 bmax 30] @?= bmax
    ]

  testset1_general = testGroup "==== General" [
    testCase "maxBoxNum [CLR 5, INC 10, JEQ 40 30 20]" $
      maxBoxNum [CLR 5, INC 10, JEQ 40 30 20] @?= 40
    ]

  testset1_errors = testGroup "==== Error Handling" [
    testCase "maxBoxNum [CLR (-n)]" $
      maxBoxNum [CLR (-2)] @?= 1,
    testCase "maxBoxNum [CLR 5, INC 10, JEQ 40 30 (-i) ]" $
      maxBoxNum [CLR 5, INC 10, JEQ 40 30 (-5)] @?= 40
    ]




  -------------------------------------------------------------
  -- PROBLEM 2: show BATConfig
  -------------------------------------------------------------
  testset2 :: TestTree
  testset2 = testGroup "================ PROBLEM 2: show BATConfig" [
    testset2_boundaries,
    testset2_general,
    testset2_errors
    ]


  testset2_boundaries = testGroup "==== Boundaries" [
    testCase "{boxes = [], counter = 0}" $
      show cfg0{boxes = [], counter = 0} @?= "boxes = []; counter = 0",
    testCase "{boxes = [], counter = 12}" $
      show cfg0{boxes = [], counter = 12} @?= "boxes = []; counter = 12",
    testCase "{boxes = [5,4,3,2,1], counter = 0}" $
      show cfg0{boxes = [5,4,3,2,1], counter = 0} @?= "boxes = [5,4,3,2,1]; counter = 0"
    ]

  testset2_general = testGroup "==== General" [
    testCase "{boxes = [1,2,3,4,5], counter = 20}"  $
      show cfg0{boxes = [1,2,3,4,5], counter = 20} @?= "boxes = [1,2,3,4,5]; counter = 20"
    ]

  testset2_errors = testGroup "==== Error Handling" [
    ]


  -------------------------------------------------------------
  -- PROBLEM 3: initialise
  -------------------------------------------------------------
  testset3 = testGroup "================ PROBLEM 3: initialise" [
    testset3_boundaries,
    testset3_general,
    testset3_errors
    ]


  testset3_boundaries = testGroup "==== Boundaries" [
    testCase "  boxes: initialise [] []" $
      boxes (initialise [] []) @?= [0, 0],
    testCase "counter: initialise [] []" $
      counter (initialise [] []) @?= 0,
    testCase "  boxes: initialise [] [3,6,9]" $
      boxes (initialise [] [3, 6, 9]) @?= [0, 3, 6, 9],
    testCase "counter: initialise [] [3,6,9]" $
      counter (initialise [] [3, 6, 9]) @?= 0,
    testCase "  boxes: initialise [CLR 5] []" $
      boxes (initialise [CLR 5] []) @?= [0, 0, 0, 0, 0, 0],
    testCase "counter: initialise [CLR 5] []" $
      counter (initialise [CLR 5] []) @?= 0,
    testCase "  boxes: initialise [CLR 0] [1,2,3]" $
      boxes (initialise [CLR 0] [1, 2, 3]) @?= [0, 1, 2, 3],
    testCase "counter: initialise [CLR 0] [1,2,3]" $
      counter (initialise [CLR 0] [1, 2, 3]) @?= 0
    ]

  testset3_general = testGroup "==== General" [
    testCase "  boxes: initialise [CLR 5, CLR 6, CLR 1] [1,2]" $
      boxes (initialise [CLR 5, CLR 6, CLR 1] [1,2]) @?= [0,1,2,0,0,0,0],
    testCase "counter: initialise [CLR 5, CLR 6, CLR 1] [1,2]" $
      counter (initialise [CLR 5, CLR 6, CLR 1] [1, 2]) @?= 0,
    testCase "  boxes: initialise [CLR 1] [1,2]" $
      boxes (initialise [CLR 1] [1, 2]) @?= [0, 1, 2],
    testCase "counter: initialise [CLR 1] [1,2]" $
      counter (initialise [CLR 1] [1, 2]) @?= 0
    ]

  testset3_errors = testGroup "==== Error Handling" [
    testCase "  boxes: initialise [CLR (-1)] [1,2]" $
      boxes (initialise [CLR (-1)] [1, 2]) @?= [0, 1, 2],
    testCase "counter: initialise [CLR (-1)] [1,2]" $
      counter (initialise [CLR (-1)] [1, 2]) @?= 0
    ]



  -------------------------------------------------------------
  -- PROBLEM 4: acceptState
  -------------------------------------------------------------
  testset4 = testGroup "================ PROBLEM 4: acceptState" [
    testset4_boundaries,
    testset4_general,
    testset4_errors
    ]


  testset4_boundaries = testGroup "==== Boundaries" [
    testCase "acceptState []: {counter = 0}" $
      acceptState [] cfg0{boxes = [7], counter = 0} @?= True,
    testCase "acceptState []: {counter = 1}" $
      acceptState [] cfg0{boxes = [0], counter = 0} @?= True,
    testCase "acceptState [CLR 5]: {counter = 0}" $
      acceptState [CLR 5] cfg0{counter = 0} @?= False,
    testCase "acceptState [CLR 5]: {counter = 1}" $
      acceptState [CLR 5] cfg0{counter = 1} @?= True
    ]

  testset4_general = testGroup "==== General" [
    testCase "acceptState [CLR 0, CLR 1, JEQ 1 2 3]: {counter = 0}" $
      acceptState [CLR 0, CLR 1, JEQ 1 2 3] cfg0{counter = 0} @?= False,
    testCase "acceptState [CLR 0, CLR 1, JEQ 1 2 3]: {counter = 1}" $
      acceptState [CLR 0, CLR 1, JEQ 1 2 3] cfg0{counter = 1} @?= False,
    testCase "acceptState [CLR 0, CLR 1, JEQ 1 2 3]: {counter = 2}" $
      acceptState [CLR 0, CLR 1, JEQ 1 2 3] cfg0{counter = 2} @?= False,
    testCase "acceptState [CLR 0, CLR 1, JEQ 1 2 3]: {counter = 3}" $
      acceptState [CLR 0, CLR 1, JEQ 1 2 3] cfg0{counter = 3} @?= True,
    testCase "acceptState [CLR 0, CLR 1, JEQ 1 2 3]: {counter = 17}" $
      acceptState [CLR 0, CLR 1, JEQ 1 2 3] cfg0{counter = 17} @?= True
    ]

  testset4_errors = testGroup "==== Error Handling" [
    testCase "acceptState [CLR 5]: {counter = (-1)}" $
      acceptState [CLR 5] cfg0{counter = -1} @?= True
    ]


  -------------------------------------------------------------
  -- PROBLEM 5: doNextMove
  -------------------------------------------------------------
  testset5 = testGroup "================ PROBLEM 4: doNextMove" [
    testset5_boundaries,
    testset5_general,
    testset5_errors
    ]

  cfg00 = initialise [] [1,2,3,4] :: BATConfig
  cfg01 = doNextMove [] cfg00

  cfg10 = initialise [CLR 5] [1,2,3,4] :: BATConfig
  cfg11 = doNextMove [CLR 5] cfg10
  cfg12 = doNextMove [CLR 5] cfg11

  cfg20 = initialise [INC 2, JEQ 1 1 0] [1,2,3,4] :: BATConfig
  cfg21 = doNextMove [INC 2, JEQ 1 1 0] cfg20
  cfg22 = doNextMove [INC 2, JEQ 1 1 0] cfg21
  cfg23 = doNextMove [INC 2, JEQ 1 1 0] cfg22

  testset5_boundaries = testGroup "==== Boundaries" [
    testCase "doNextMove [] (initialise _ [1,2,3,4])" $
      cfg01 @?= cfg00
    ]

  testset5_general = testGroup "==== General" [
    testCase "doNextMove [CLR 5] (initialise _ [1,2,3,4])" $
      cfg11 @?= cfg10{boxes=[0,1,2,3,4,0], counter=1},
    testCase "doNextMove" $
      cfg12 @?= cfg11, -- HALT
    testCase "doNextMove [INC 2, JEQ 1 1 0] (initialise _ [1,2,3,4])" $
      cfg21 @?= cfg20{counter=1, boxes=[0,1,3,3,4]},
    testCase "doNextMove" $
      cfg22 @?= cfg20{counter=0, boxes=[0,1,3,3,4]},
    testCase "doNextMove" $
      cfg23 @?= cfg20{counter=1, boxes=[0,1,4,3,4]}
    ]

  testset5_errors = testGroup "==== Error Handling" [
    ]


  -------------------------------------------------------------
  -- PROBLEM 6: runFrom
  -------------------------------------------------------------
  testset6 = testGroup "================ PROBLEM 6: runFrom" [
    testset6_boundaries,
    testset6_general,
    testset6_errors
    ]

  -- p0 = []
  -- p1 = [CLR 5]
  -- p2 = [INC 2, JEQ 1 1 0]

  -- cfg00 = initialise p0 [1,2,3,4] :: BATConfig
  run00 = runFrom [] cfg00
  run01 = runFrom [] cfg01

  -- cfg10 = initialise p1 [1,2,3,4] :: BATConfig
  run10 = runFrom [CLR 5] cfg10
  run11 = runFrom [CLR 5] cfg11

  -- cfg20 = initialise p2 [1,2,3,4] :: BATConfig
  run20 = runFrom [INC 2, JEQ 1 1 0] cfg20 -- loops!



  testset6_boundaries = testGroup "==== Boundaries" [
    testCase "runFrom [] (initialise)" $
      run00 @?= cfg00,
    testCase "runFrom [] (next)" $
      run01 @?= cfg00
    ]

  testset6_general = testGroup "==== General" [
    testCase "runFrom [CLR 5] (initialise)" $
      run10 @?= cfg11,
    testCase "runFrom [CLR 5] (next)" $
      run11 @?= cfg11
    ]

  testset6_errors = testGroup "==== Error Handling" [
    ]


  -------------------------------------------------------------
  -- PROBLEM 7: getOutput
  -------------------------------------------------------------
  testset7 = testGroup "================ PROBLEM 7: getOutput" [
    testset7_boundaries,
    testset7_general,
    testset7_errors
    ]

  testset7_boundaries = testGroup "==== Boundaries" [
    testCase "getOutput: {boxes = [4]}" $
      getOutput cfg00{boxes = [4]} @?= 0,
    testCase "getOutput: {boxes = [4,5]}" $
      getOutput cfg00{boxes = [4, 5]} @?= 5
    ]

  testset7_general = testGroup "==== General" [
    testCase "getOutput: {boxes = [2,3,5,2]}" $
      getOutput cfg00{boxes = [2, 3, 5, 2]} @?= 3
    ]

  testset7_errors = testGroup "==== Error Handling" [
    testCase "getOutput: {boxes = []}" $
      getOutput(cfg00{boxes=[]}) @?= 0
    ]


  -------------------------------------------------------------
  -- PROBLEM 8: transpose
  -------------------------------------------------------------
  testset8 = testGroup "================ PROBLEM 8: transpose" [
    testset8_boundaries,
    testset8_general,
    testset8_errors
    ]

  testset8_boundaries = testGroup "==== Boundaries" [
    testCase "transpose 0 [JEQ 5 6 7]" $
      transpose 0 [JEQ 5 6 7] @?= [JEQ 5 6 7]
    ]

  testset8_general = testGroup "==== General" [
    testCase "transpose 5 []" $
      transpose 5 [] @?= [],
    testCase "transpose 5 [CLR 7]" $
      transpose 5 [CLR 7] @?= [CLR 7],
    testCase "transpose 7 [CLR 1, JEQ 1 1 6]" $
      transpose 7 [CLR 1, JEQ 1 1 6] @?= [CLR 1, JEQ 1 1 13]
    ]

  testset8_errors = testGroup "==== Error Handling" [
    testCase "transpose (-3) [JEQ 5 6 7]" $
      transpose (-3) [JEQ 5 6 7] @?= [JEQ 5 6 4]
    ]


  -------------------------------------------------------------
  -- PROBLEM 9: (*->*)
  -------------------------------------------------------------
  testset9 = testGroup "================ PROBLEM 9: (*->*)" [
    testset9_boundaries,
    testset9_general,
    testset9_errors
    ]

  testset9_boundaries = testGroup "==== Boundaries" [
    testCase "[] *->* []" $
      ([] *->* []) @?= [],
    testCase "[] *->* [JEQ 1 1 0]" $
      ([] *->* [JEQ 1 1 0]) @?= [JEQ 1 1 0],
    testCase "[JEQ 1 1 0] *->* []" $
      ([JEQ 1 1 0] *->* []) @?= [JEQ 1 1 0]
    ]

  testset9_general = testGroup "==== General" [
    testCase "[JEQ 1 2 3] *->* [JEQ 2 2 8]" $
      ([JEQ 1 2 3] *->* [JEQ 2 2 8]) @?= [JEQ 1 2 3, JEQ 2 2 9]
    ]

  testset9_errors = testGroup "==== Error Handling" [
    ]



  -------------------------------------------------------------
  -- PROBLEM 10: adder
  -------------------------------------------------------------
  testset10 = testGroup "================ PROBLEM 10: adder" [
    testset10_boundaries,
    testset10_general,
    testset10_errors
    ]

  testset10_boundaries = testGroup "==== Boundaries" [
    testCase "execute adder [3]" $
      execute adder [3] @?= 3
    ]

  testset10_general = testGroup "==== General" [
    testCase "execute adder [3,4,5]" $
      execute adder [3, 4, 5] @?= 7,
    testCase "execute adder [123,876]" $
      execute adder [123, 876] @?= 999
    ]

  testset10_errors = testGroup "==== Error Handling" [
    testCase "execute adder []" $
      execute adder [] @?= 0
    ]



  -------------------------------------------------------------
  -- PROBLEM 11: copyBox
  -------------------------------------------------------------
  testset11 = testGroup "================ PROBLEM 11: copyBox" [
    testset11_boundaries,
    testset11_general,
    testset11_errors
    ]

  testset11_boundaries = testGroup "==== Boundaries" [
    testCase "boxes $ runProgram (copyBox 0 0) [3]" $
      boxes (runProgram (copyBox 0 0) [3]) @?= [0, 3],
    testCase "boxes $ runProgram (copyBox 0 1) [3]" $
      boxes (runProgram (copyBox 0 1) [3]) @?= [0, 0],
    testCase "boxes $ runProgram (copyBox 1 0) [3]" $
      boxes (runProgram (copyBox 1 0) [3]) @?= [3, 3],
    testCase "boxes $ runProgram (copyBox 1 1) [3]" $
      boxes (runProgram (copyBox 1 1) [3]) @?= [0, 3]
    ]

  testset11_general = testGroup "==== General" [
    testCase "boxes $ runProgram (copyBox 1 2) [3,4,5,6]" $
      boxes (runProgram (copyBox 1 2) [3, 4, 5, 6]) @?= [0, 3, 3, 5, 6]
    ]

  testset11_errors = testGroup "==== Error Handling" [
    testCase "boxes $ runProgram (copyBox 0 0) []" $
      boxes (runProgram (copyBox 0 0) []) @?= [0, 0],
    testCase "boxes $ runProgram (copyBox 1 2) []" $
      boxes (runProgram (copyBox 1 2) []) @?= [0, 0, 0]
    ]



  -------------------------------------------------------------
  -- PROBLEM 12: addXY
  -------------------------------------------------------------
  testset12 = testGroup "================ PROBLEM 12: addXY" [
    testset12_boundaries,
    testset12_general,
    testset12_errors
    ]

  testset12_boundaries = testGroup "==== Boundaries" [
    testCase "(addXY 0 0) []" $
      execute (addXY 0 0) [] @?= 0,
    testCase "(addXY 0 0) [7]" $
      execute (addXY 0 0) [7] @?= 0,
    testCase "(addXY 1 1) [7]" $
      execute (addXY 1 1) [7] @?= 14
    ]

  testset12_general = testGroup "==== General" [
    testCase "(addXY 1 2) [3,4]" $
      execute (addXY 1 2) [3, 4] @?= 7,
    testCase "(addXY 3 2) [5,6,7,8]" $
      execute (addXY 3 2) [5, 6, 7, 8] @?= 13,
    testCase "(addXY 2 2) [7,9]" $
      execute (addXY 2 2) [7, 9] @?= 18
    ]

  testset12_errors = testGroup "==== Error Handling" [
    testCase "(addXY 3 2) []" $
      execute (addXY 3 2) [] @?= 0
    ]



  -------------------------------------------------------------
  -- ADVANCED -- addXY leaves all other boxes unchanged?
  -------------------------------------------------------------
  addXYAdvanced = testGroup "================ ADVANCED" [
    testGroup "===== addXY leaves other boxes unchanged?" [
      testCase "addXY 0 0" $
        boxes (runProgram (addXY 0 0) (3:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]))
           @?= boxes (runProgram [] (0:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20])),
      testCase "addXY 1 1" $
        boxes (runProgram (addXY 1 1) (3:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]))
          @?= boxes (runProgram [] (6:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20])),
      testCase "addXY 2 3" $
        boxes (runProgram (addXY 2 3) (3:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]))
          @?= boxes (runProgram [] (9:[4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]))
      ]
    ]



  ---------------------------------------------------------------
  -- BUILD AND RUN GLOBAL TEST TREE (run "main" to execute tests)
  ---------------------------------------------------------------
  testsets = testGroup ""
    [testset1, testset2, testset3, testset4,  testset5,  testset6,
     testset7, testset8, testset9, testset10, testset11, testset12,
     addXYAdvanced]

  main = defaultMain $
             localOption (mkTimeout 1000000) testsets


  ---------------------------------------------------------------
  --                   END OF TEST HARNESS
  --                   Mike Stannett, 2018
  --                m.stannett@sheffield.ac.uk
  ---------------------------------------------------------------

  -- END OF TEST CASES

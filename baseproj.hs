-- Type & data declarations
type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

-- Helper Functions

--(delete item from list)
delItem _ [] = []
delItem x (y:ys) | x == y = delItem x ys
                 | otherwise = y : delItem x ys

--(use in nextMyStates to check if applying up/down/right/left returns null or not)
produceNextStates f	| f == Null = []
					| otherwise = [f]


-- Main Code

--up
up :: MyState -> MyState
up Null = Null
up (S (x,y) list string prevstate) | x>0 = (S (x-1,y) list "up" (S (x,y) list string prevstate) )
								   | otherwise = Null

--down
down :: MyState -> MyState
down Null = Null
down (S (x,y) list string prevstate) | x<3 = (S (x+1,y) list "down" (S (x,y) list string prevstate) )
									 | otherwise = Null

--left
left :: MyState -> MyState
left Null = Null
left (S (x,y) list string prevstate) | y>0 = (S (x,y-1) list "left" (S (x,y) list string prevstate) )
									 | otherwise = Null

--right
right :: MyState -> MyState
right Null = Null
right (S (x,y) list string prevstate) | y<3 = (S (x,y+1) list "right" (S (x,y) list string prevstate) )
									  | otherwise = Null

--collect
collect :: MyState -> MyState
collect Null = Null
collect (S (x,y) list string prevstate) | elem (x,y) list = (S (x,y) (delItem (x,y) list) "collect" (S (x,y) list string prevstate) )
										| otherwise = Null

--nextMyStates
nextMyStates :: MyState -> [MyState]
nextMyStates Null = []
nextMyStates inpState = produceNextStates (up inpState) ++ produceNextStates (down inpState) ++ produceNextStates (left inpState) ++ produceNextStates (right inpState) ++ produceNextStates (collect inpState)

--isGoal
isGoal :: MyState -> Bool
isGoal Null = False
isGoal (S (x,y) list string prevstate) | (length list == 0) = True
									   | otherwise = False

--search
search :: [MyState] -> MyState
search [] = Null
search (y:ys) | isGoal y = y
			  | otherwise = search ( ys ++ (nextMyStates y) )

--constructSolution
constructSolution state = reverse(h state)
h (S (y,x) l s p) | s=="" = []
                       | otherwise = s:h p

--solve
solve :: Cell -> [Cell] -> [String]
solve (x,y) l = constructSolution (search  [(S (x,y) l "" Null)])

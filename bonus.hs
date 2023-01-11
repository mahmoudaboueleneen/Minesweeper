type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)
--the grid size variable changes the length of the grid to be gridSizeXgridSize
gridSize::Int
gridSize = 20


up (S cell mines lastAction previousState) = 
 if x-1>=0 then (S  (x-1,y) mines "up" (S  cell mines lastAction previousState))	
 else Null
    where (x,y) = cell

down (S cell mines lastAction previousState) = 
 if x+1<gridSize then (S  (x+1,y) mines "down" (S  cell mines lastAction previousState))	
  else Null
 where (x,y) = cell

right (S cell mines lastAction previousState) = 
	if y+1<gridSize then (S  (x,y+1) mines "right" (S  cell mines lastAction previousState))	
  else Null
  where (x,y) = cell

left (S cell mines lastAction previousState) = 
	 if y-1>=0 then (S  (x,y-1) mines "left" (S  cell mines lastAction previousState))	
	 else Null
	where (x,y) = cell

collect (S cell mines lastAction previousState) = 
   if (elem cell mines) == True then 
				 (S cell (filter (/= cell) mines) "collect" (S cell mines lastAction previousState))	
						 else Null

closestNextStateMove (S cell mines lastAction previousState) = nextMyStatesHelper (closestNextStateMoveHelper cell closest) (S  cell mines lastAction previousState)
			 where closest = getClosestMine cell mines
closestNextStateMoveHelper (xCurrentCell,yCurrentCell) (xClosest,yClosest) = if(xCurrentCell==xClosest&&yCurrentCell==yClosest) then ["collect"]
		else if(xCurrentCell>xClosest) then ["up"]
		 else if(xCurrentCell<xClosest) then ["down"]
		else if(yCurrentCell<yClosest) then ["right"]
	 else if(yCurrentCell>yClosest) then ["left"]	
    else []
	
getClosestMine (xCell,yCell) (x:xs) = getClosestMineHelper (xCell,yCell) xs (xFirst,yFirst)
   where (xFirst,yFirst) = x
getClosestMineHelper (xOriginal,yOriginal) [] (xLeastSoFar,yLeastSoFar)= (xLeastSoFar,yLeastSoFar)
getClosestMineHelper (xOriginal,yOriginal) (currentMine:restofMInes) (xLeastSoFar,yLeastSoFar)= if (abs(xCurrentCell-xOriginal)+abs(yCurrentCell-yOriginal)) <(abs(yLeastSoFar-yOriginal)+abs(xLeastSoFar-xOriginal)) then getClosestMineHelper (xOriginal,yOriginal) restofMInes (xCurrentCell,yCurrentCell)
  else getClosestMineHelper (xOriginal,yOriginal) restofMInes (xLeastSoFar,yLeastSoFar)
 where (xCurrentCell,yCurrentCell) = currentMine
nextMyStates state = 
	(nextMyStatesHelper ["up","down","left","right","collect"] state)

nextMyStatesHelper [] _ = []

nextMyStatesHelper (x:xs) state = 
   if (x == "up") then if (up state /= Null) then (up state):(nextMyStatesHelper xs state)
	 else nextMyStatesHelper xs state
	else if(x == "down") then if (down state /= Null) then (down state):(nextMyStatesHelper xs state)
  else nextMyStatesHelper xs state
 else if (x=="left") then if (left state /= Null) then (left state):(nextMyStatesHelper xs state)
 else nextMyStatesHelper xs state
 else if(x =="right") then if (right state /= Null) then (right state):(nextMyStatesHelper xs state)
  else nextMyStatesHelper xs state
  else if (collect state /= Null) then (collect state):(nextMyStatesHelper xs state)
	else nextMyStatesHelper xs state


isGoal (S _ [] _ _) = True
isGoal (S _ (x:xs) _ _) = False

search (state:restOfStates) = if(isGoal state == True) then state
 else search (restOfStates ++ closestNextStateMove state)
constructSolution:: MyState ->[String]
constructSolution (S _ _ "" Null) = []
constructSolution (S _ _ action previousState) = (constructSolution previousState)++[action]
solve::Cell->[Cell]->[String]
solve startCell mines = constructSolution(search([S startCell mines "" Null]))
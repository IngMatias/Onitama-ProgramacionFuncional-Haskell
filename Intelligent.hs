module Intelligent where

import Onitama
import GameFunctions

import Data.Maybe (fromJust)
import Data.List (elemIndex)

winner :: [GameResult OnitamaPlayer] -> OnitamaPlayer
winner [ _ , Winner player ] = player 

runPossibleMatch :: (OnitamaAgent, OnitamaAgent) -> OnitamaGame -> IO OnitamaPlayer
runPossibleMatch ags@(ag1, ag2) g = do
   case (activePlayer g) of
      Nothing -> return (winner (result g))
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runPossibleMatch ags (GameFunctions.next g p (fromJust move))

runRandomPossibleMatch :: OnitamaGame -> IO OnitamaPlayer
runRandomPossibleMatch g = do
   runPossibleMatch (randomAgent RedPlayer, randomAgent BluePlayer) g

toIOInt :: OnitamaPlayer -> IO OnitamaPlayer -> IO Int
toIOInt player playerIO  =  do
    pla <- playerIO
    return (if (pla == player) then 1 else 0)

possibilityOf :: OnitamaGame -> OnitamaAction -> IO Double
possibilityOf (GameState player cards table config) action = do
    binarList <- mapM (toIOInt player) [runRandomPossibleMatch (GameFunctions.next (GameState player cards table config) player action) | x<-[0..5]]
    return (fromIntegral (sum binarList) / 5)

listActions :: OnitamaGame -> [OnitamaAction]
listActions game = concat[if(not (null action)) then action else []| (player,action) <- (actions game)]

makePossibiliyTuple :: OnitamaGame -> IO [(Double, OnitamaAction)]
makePossibiliyTuple game = do
    let listPossibleAction = listActions game
    possibilityList <- (mapM (possibilityOf game) listPossibleAction)
    return (zip possibilityList listPossibleAction)

maxProbability :: IO [(Double, OnitamaAction)] -> IO (Double, OnitamaAction)
maxProbability list = do
    noIOList<-list
    return (maximum noIOList)

takeDesition :: OnitamaGame -> IO OnitamaAction
takeDesition game = do
    tuple <- (maxProbability (makePossibiliyTuple game))
    return (snd tuple)



intelligentAgent :: OnitamaPlayer -> OnitamaAgent
intelligentAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       desition <- (takeDesition state)
       return (Just(desition))
    
runInteligentGame :: IO [GameResult OnitamaPlayer]
runInteligentGame = do
   runGame (intelligentAgent RedPlayer, intelligentAgent BluePlayer)

runTestGame :: IO [GameResult OnitamaPlayer]
runTestGame = do
   runGame (intelligentAgent RedPlayer, randomAgent BluePlayer)

module IntelligentAgent where

import Onitama
import GameFunctions

import Data.Maybe (fromJust)
import Data.List (elemIndex)

-- Recibe un resultado de juego.
-- Retorna el ganador.
winner :: [GameResult OnitamaPlayer] -> OnitamaPlayer
winner [ _ , Winner player ] = player

-- Recibe dos agentes de juego y un estado de juego.
-- Retorna el ganador de esa partida.
runPossibleMatch :: (OnitamaAgent, OnitamaAgent) -> OnitamaGame -> IO OnitamaPlayer
runPossibleMatch ags@(ag1, ag2) g = do
   case (activePlayer g) of
      Nothing -> return (winner (result g))
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runPossibleMatch ags (GameFunctions.next g p (fromJust move))

-- Recibe un estado del juego.
-- Termina la partida retornando el ganador.
runRandomPossibleMatch :: OnitamaGame -> IO OnitamaPlayer
runRandomPossibleMatch g = do
   runPossibleMatch (randomAgent RedPlayer, randomAgent BluePlayer) g

-- Recibe un jugador y un IO jugador.
-- Retorna 1 si son iguales y 0 en caso contrario.
toIOInt :: OnitamaPlayer -> IO OnitamaPlayer -> IO Int
toIOInt player playerIO  =  do
    pla <- playerIO
    return (if (pla == player) then 1 else 0)

-- Recibe un juego y una accion.
-- Retorna la posibilidad de ganar, tras jugar la accion.
possibilityOf :: OnitamaGame -> OnitamaAction -> IO Double
possibilityOf (GameState player cards table config) action = do
    binarList <- mapM (toIOInt player) [runRandomPossibleMatch (GameFunctions.next (GameState player cards table config) player action) | x<-[0..5]]
    return (fromIntegral (sum binarList) / 5)

-- Recibe un juego.
-- Retorna las posibles acciones del jugador.
listActions :: OnitamaGame -> [OnitamaAction]
listActions game = concat[if(not (null action)) then action else []| (player,action) <- (actions game)]

-- Recibe un juego.
-- Retorna una lista de tuplas, donde su primer coordenada es la posibilidad de ganar aplicando la accion, que es su segunda coordenada.
makePossibiliyTuple :: OnitamaGame -> IO [(Double, OnitamaAction)]
makePossibiliyTuple game = do
    let listPossibleAction = listActions game
    possibilityList <- (mapM (possibilityOf game) listPossibleAction)
    return (zip possibilityList listPossibleAction)

-- Toma una lista de tuplas.
-- Retorna aquella cuya primer coordenada es maxima.
maxProbability :: IO [(Double, OnitamaAction)] -> IO (Double, OnitamaAction)
maxProbability list = do
    noIOList<-list
    return (maximum noIOList)

-- Toma un juego.
-- Retorna la accion con mas probabilidades de ganar.
takeDesition :: OnitamaGame -> IO OnitamaAction
takeDesition game = do
    tuple <- (maxProbability (makePossibiliyTuple game))
    return (snd tuple)

-- Recibe un jugador y un estado del juego.
-- Retorna la accion con mas probabilidades de ganar.
intelligentAgent :: OnitamaPlayer -> OnitamaAgent
intelligentAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       desition <- (takeDesition state)
       return (Just(desition))

-- Inicia un juego entre jugadores inteligentes.
runIntelligentGame :: IO [GameResult OnitamaPlayer]
runIntelligentGame = do
   runGame (intelligentAgent RedPlayer, intelligentAgent BluePlayer)

-- Inicia un juego jugador inteligente vs jugador randomico.
runIntelligentVsRandomGame :: IO [GameResult OnitamaPlayer]
runIntelligentVsRandomGame = do
   runGame (intelligentAgent RedPlayer, randomAgent BluePlayer)

-- Inicia un juego singleplayer contra el jugador inteligente.
runIntelligentVsConsoleGame :: IO [GameResult OnitamaPlayer]
runIntelligentVsConsoleGame = do
    runGame (consoleAgent RedPlayer, intelligentAgent BluePlayer)
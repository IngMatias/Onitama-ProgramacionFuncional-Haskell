
module VariantFunctions where

import GameFunctions
import Onitama
import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex, sort)
import System.Random

variant :: OnitamaConfig -> OnitamaGame
variant config = beginningVariant config (configDeck config)

config :: OnitamaConfig
config = OnitamaConfig deck 7 False

--configStalemate en True indica que el jugador sin movimientos pierde.
--configStalemate en False indica que el jugador sin movimientos pasa.
data OnitamaConfig = OnitamaConfig { configDeck :: [OnitamaCard], configHandSize :: Int, configStalemate :: Bool } 
    deriving (Eq, Show)

beginningVariant :: OnitamaConfig -> [OnitamaCard] -> OnitamaGame
beginningVariant config cards = GameState RedPlayer (take (1+2*(configHandSize config)) cards) initTable

-- Toma un estado del juego.
-- Retorna una lista de tuplas (player, posibles jugadas)
-- Si no es el turno del jugador retorna la lista vacia.
-- Si no tiene acciones se retorna la lista con NoMovementAction
actionsVariant :: OnitamaGame -> OnitamaConfig -> [(OnitamaPlayer, [OnitamaAction])]
actionsVariant (GameState player cards table) config
    | endedGame table = []
    | otherwise = [(player , if (null actions && not (configStalemate config)) then [NoMovementAction] else actions) , (otherPlayer player, [])]
    where actions = possibleActions (GameState player (playerCards player cards) table)

-- Si se recibe la NoMovementAcrion solo se actualiza la lista de cartas
-- En caso contrario, se usa el next normal
nextVariant :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
nextVariant (GameState player cards table ) playerMover NoMovementAction
    | (player /= playerMover) = error "No es el turno del jugador."
    | otherwise = (GameState (otherPlayer player) (nextCards cards cardToMove) table)
    where cardToMove = if (player==RedPlayer) then (head cards) else (cards !! (div (length cards - 1) 2))
nextVariant game player action = GameFunctions.next game player action




-- Se usa el codigo entregado para escribir funciones de juego con variante
-- La función ´runVariantMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
-- agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
runVariantMatch :: (OnitamaAgent, OnitamaAgent) -> OnitamaGame -> IO [GameResult OnitamaPlayer]
runVariantMatch ags@(ag1, ag2) g = do
   putStrLn (showGame g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         -- Llamo a nextVariant
         runVariantMatch ags (VariantFunctions.nextVariant g p (fromJust move))

runVariantGame :: (OnitamaAgent, OnitamaAgent) -> IO [GameResult OnitamaPlayer]
runVariantGame ags = do
  cards <- shuffle deck
  -- Llamo a beginningVariant
  runVariantMatch ags (beginningVariant config cards)

-- El agente de consola ´consoleVariantAgent´ muestra el estado de juego y los movimientos disponibles por
-- consola, y espera una acción por entrada de texto.
consoleVariantAgent :: OnitamaPlayer -> OnitamaAgent
consoleVariantAgent player state = do
   let moves = fromJust (lookup player (actionsVariant state config))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ showAction m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleVariantAgent player state

-- Las funciones ´runConsoleGame´ y `runConsoleMatch` ejecutan toda la partida 
-- usando dos agentes de consola.
runConsoleVariantGame :: IO [GameResult OnitamaPlayer]
runConsoleVariantGame = do
   runGame (consoleVariantAgent RedPlayer, consoleVariantAgent BluePlayer)
runConsoleVariantMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runConsoleVariantMatch g = do
   runMatch (consoleVariantAgent RedPlayer, consoleVariantAgent BluePlayer) g

-- El agente aleatorio ´randomAgent´ elige una acción de las disponibles completamente al azar.
randomVariantAgent :: OnitamaPlayer -> OnitamaAgent
randomVariantAgent player state = do
    let moves = fromJust (lookup player (actionsVariant state config))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

-- Las funciones ´runRandomGame´ y `runRandomMatch` ejecutan toda la partida 
-- usando dos agentes aleatorios.
runRandomVariantGame :: IO [GameResult OnitamaPlayer]
runRandomVariantGame = do
   runVariantGame (randomVariantAgent RedPlayer, randomVariantAgent BluePlayer)
runRandomVariantMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runRandomVariantMatch g = do
   runVariantMatch (randomVariantAgent RedPlayer, randomVariantAgent BluePlayer) g


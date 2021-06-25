
module Juego where
-- Almacena la informacion del estado del juego en cualquier momento.
-- El jugador es el siguiente en mover.
-- Las dos primeras cartas corresponden al jugador rojo, 
-- las dos siguientes al jugador azul, la ultima es la carta en espera.
data OnitamaGame = GameState OnitamaPlayer [OnitamaCard] OnitamaTable
    deriving (Eq, Show)

-- Tablero.
type OnitamaTable = [[OnitamaPiece]]

-- La primera coordenada corresponde a la columna, la segunda a la fila.
-- (0,0) corresponde a la esquina izquierda superior del tablero.
-- (4,0) corresponde a la esquina derecha superior del tablero.
-- (0,4) corresponde a la esquina izquierda inferior del tablero.
-- (4,4) corresponde a la esquina derecha inferior del tablero.
type Coordinate = (Int,Int)

-- Movimiento de una pieza.
-- La primera coordenada corresponde cuantos casilleros hacia la derecha.
-- La segunda coordenada corresponde cuantos casilleros hacia adelante.
-- Ambas pueden ser negativas, mostrando casilleros hacia atras y hacia la izquierda.
type Movement = (Int,Int)

-- Almacena un posible movimiento del jugador.
-- El primer par de coordenadas indica donde esta posicionada la pieza.
-- El segundo par de coordenadas indica donde estara posicionada la pieza al final del movimiento.
data OnitamaAction = Action OnitamaPiece OnitamaCard Coordinate Coordinate
    deriving (Eq, Show, Read)

-- Piezas posibles para jugar.
data OnitamaPiece = Master OnitamaPlayer | Apprentice OnitamaPlayer | NoPiece
    deriving (Eq, Show, Read)

-- Posibles cartas para jugar.
data OnitamaCard = 
                Tiger   | Dragon    | Frog  | Rabbit    |
                Crab    | Elephant  | Goose | Rooster   |
                Monkey  | Mantis    | Horse | Ox        |
                Crane   | Boar      | Eel   | Cobra     
    deriving (Eq, Show, Enum, Read)

-- Jugadores (Prof).
data OnitamaPlayer = RedPlayer | BluePlayer | NoPlayer
    deriving (Eq, Show, Enum, Read, Bounded)

-- Posibles resultados de la partida (Prof).
data GameResult p = Winner p | Loser p | Draw
    deriving (Eq, Show)

-- Recibe las cartas ya barajadas.
-- Retorna el estado incial del juego.
beginning :: [OnitamaCard] -> OnitamaGame
beginning cards = GameState RedPlayer (take 5 cards) initTable
    where initTable = [
            [Apprentice RedPlayer,    NoPiece,    NoPiece,    NoPiece,    Apprentice BluePlayer],
            [Apprentice RedPlayer,    NoPiece,    NoPiece,    NoPiece,    Apprentice BluePlayer],
            [Master RedPlayer,        NoPiece,    NoPiece,    NoPiece,    Master BluePlayer],
            [Apprentice RedPlayer,    NoPiece,    NoPiece,    NoPiece,    Apprentice BluePlayer],
            [Apprentice RedPlayer,    NoPiece,    NoPiece,    NoPiece,    Apprentice BluePlayer]]

-- Recibe un estado del juego.
-- Retorna el proximo jugador en mover.
-- activePlayer :: OnitamaGame -> Maybe OnitamaPlayer
-- activePlayer (GameState player cards table)
--    | endedGame table = Nothing
--    | otherwise = Just (otherPlayer player)

-- Recibe un jugador.
-- Retorna el otro jugador.
otherPlayer :: OnitamaPlayer -> OnitamaPlayer
otherPlayer (RedPlayer) = BluePlayer
otherPlayer (BluePlayer) = RedPlayer

-- Recibe un jugador y una lista de cartas.
-- Retorna las cartas del jugador.
playerCards :: OnitamaPlayer -> [OnitamaCard] -> [OnitamaCard]
playerCards RedPlayer [c1,c2,c3,c4,c5] = [c1,c2]
playerCards BluePlayer [c1,c2,c3,c4,c5] =  [c3,c4]
playerCards _ cards = error ("Se esperaban 5 cartas " ++ show cards)

-- Recibe una carta.
-- Retorna todos sus posibles movimientos.
possibleMovements :: OnitamaCard -> [Movement]
possibleMovements Tiger =       [(0,-1) , (0,2)] 
possibleMovements Crab =        [(-2,0), (2,0), (0,1)]
possibleMovements Monkey =      [(1,1),(-1,-1),(1,-1),(-1,1)]
possibleMovements Crane =       [(-1, -1), (1, -1), (0, 1)]
possibleMovements Dragon =      [ (-2, 1), (-1, -1),  (1, -1), (2,1)]
possibleMovements Elephant =    [(1,0), (-1,0), (1,1), (-1,1)]
possibleMovements Mantis =      [(0,-1), (-1,1),  (1,1)]
possibleMovements Boar =        [(-1,0), (1,0), (0,1)]
possibleMovements Frog =        [(-2,0), (-1,1), (1,-1)]
possibleMovements Goose =       [(-1,1), (1,-1), (-1,0), (1,0)]
possibleMovements Horse =       [(-1,0), (0,1), (0,-1)]
possibleMovements Eel =         [(-1,1), (-1,-1), (1,0)]
possibleMovements Rabbit =      [(1,1), (-1,-1), (2,0)]
possibleMovements Rooster =     [(-1,-1),(1,1), (-1,0), (1,0)]
possibleMovements Ox =          [(1,0), (0,1), (0,-1)]
possibleMovements Cobra =       [(1,1), (1,-1), (-1,0)]

-- Recibe un par de coordenadas y un tablero.
-- Retorna si esta vacio ese lugar en el tablero.
isEmpty :: Coordinate -> OnitamaTable -> Bool
isEmpty (x,y) table = (table !! y !! x) == NoPiece

-- Recibe unas coordenadas iniciales y un movimiento.
-- Retorna el resultado de aplicar ese movimiento sobre esas coordenadas desde la posicion del jugador rojo.
-- Moverse hacia adelante un lugar.
-- (0,0) (0,1) = (1,0)
-- Moverse hacia atras un lugar.
-- (1,0) (0,-1) = (0,0)
-- Moverse hacia la derecha un lugar.
-- (0,0) (1,0) = (0,1)
-- Moverse hacia la izquierda un lugar.
-- (0,1) (-1,0) = (0,0)
redMove :: Coordinate -> Movement -> Coordinate
redMove (c1,c2) (m1,m2) = (c1+m2, c2+m1)

-- Recibe unas coordenadas iniciales y un movimiento.
-- Retorna el resultado de aplicar ese movimiento sobre esas coordenadas desde la posicion del jugador azul.
-- Moverse hacia adelante un lugar
-- (4,0) (0,1) = (3,0)
-- Moverse hacia atras un lugar
-- (3,0) (0,-1) = (4,0)
-- Moverse hacia la derecha un lugar
-- (4,1) (1,0) = (4,0)
-- Moverse hacia la izquierda un lugar
-- (4,0) (-1,0) = (4,1)
blueMove :: Coordinate -> Movement -> Coordinate
blueMove (c1,c2) (m1,m2) = (c1-m2, c2-m1)

-- Recibe una pieza.
-- Retorna su dueño.
owner :: OnitamaPiece -> OnitamaPlayer
owner (Master player) = player
owner (Apprentice player) = player
owner piece = NoPlayer

-- Recibe un jugador, un par de coordenadas y un tablero.
-- Retorna si la pieza en esas coordenadas es enemiga.
isEnemyPiece :: OnitamaPlayer -> Coordinate -> OnitamaTable -> Bool
isEnemyPiece player (x,y) table = owner (table !! y !! x) == otherPlayer player

-- Recibe un jugador (dueño de la accion), una carta, una pieza sobre la que se ejecutara la accion,
-- las coordenadas de dicha pieza y el tablero.
-- Se retornan todos los posibles resultados que dicha carta podria aplicar a la pieza.
-- Se debe respetar: el jugador no puede comer una pieza propia, si una ajena.
allPossibleResults :: OnitamaPlayer -> OnitamaCard -> OnitamaPiece -> OnitamaTable -> Coordinate -> [OnitamaAction]
allPossibleResults player card piece table coor = 
    map (\f -> Action piece card coor f) 
        ((filter (\g -> (isEmpty g table) || (isEnemyPiece player g table))
            [(x,y) | (x,y) <- (map ((if player==BluePlayer then blueMove else redMove) coor) (possibleMovements card)), 0<=x, x<=4, 0<=y, y<=4]))

-- Recibe un jugador, un mazo de cartas, una pieza, sus coordenadas y el tablero. 
-- Retorna todos los posibles resultados de la aplicacion de todas las cartas sobre la pieza.
toActions :: OnitamaPlayer -> [OnitamaCard] -> OnitamaPiece -> OnitamaTable -> Coordinate -> [OnitamaAction]
toActions _ _ NoPiece _ _ = []
toActions player cards piece table coor
    | player == (owner piece) =  (concat [allPossibleResults player card piece table coor | card <- cards])
    | otherwise = []


-- Toma un estado del juego.
-- Retorna todos los posibles resultados de la aplicacion de todas las cartas sobre todo el tablero.
possibleActions :: OnitamaGame -> [OnitamaAction]
possibleActions (GameState player cards table) = concat [(toActions player cards (table !! y !! x) table (x,y)) | y<-[0..4] , x<-[0..4] ]

-- Toma un estado del juego.
-- Retorna una lista de tuplas (player, posibles jugadas)
-- Si no es el turno del jugador retorna la lista vacia.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions (GameState player cards table) 
    | endedGame table = []
    | otherwise = [(player , possibleActions (GameState player (playerCards player cards) table)) , (otherPlayer player, [])]

-- Recibe un par de coordenadas y un tablero.
-- Retorna la pieza en ese par de coordenadas, incluso cuando no hay ninguna.  
pieceAt :: Coordinate -> OnitamaTable -> OnitamaPiece
pieceAt (x,y) table = table !! y !! x

-- Recibe una pieza.
-- Retorna si es un maestro.
isMaster :: OnitamaPiece -> Bool
isMaster (Master player) = True
isMaster _ = False

-- Recibe un tablero.
-- Retorna si se ha terminado el juego.
endedGame :: OnitamaTable -> Bool
endedGame table
    | owner (pieceAt (0,2) table) == BluePlayer = True
    | owner (pieceAt (4,2) table) == RedPlayer = True
    | length [(table !! y !! x) | x<-[0..4], y<-[0..4], isMaster (table !! y !! x)] /= 2 = True
    | otherwise = False

-- Recibe las cinco cartas en juego y una que ha sido utilizada.
-- Retorna el mazo luego del uso de la carta.
nextCards :: [OnitamaCard] -> OnitamaCard -> [OnitamaCard]
nextCards [c1,c2,c3,c4,c5] c
    | c1 == c = [c5,c2,c3,c4,c1]
    | c2 == c = [c1,c5,c3,c4,c2]
    | c3 == c = [c1,c2,c5,c4,c3]
    | c4 == c = [c1,c2,c3,c5,c4]
    | c5 == c = error "Esa carta no puede ser"

-- Recibe una fila del tablero, una pieza y un entero posicion.
-- Retorna la fila tras colocar una pieza nueva en la posicion dada, se piza la piesa anterior.
replaceRow :: [OnitamaPiece] -> OnitamaPiece -> Int -> [OnitamaPiece] 
replaceRow (x:xs) newPiece 0 = (newPiece:xs)
replaceRow [] newPiece p = error "Posicion equivocada"
replaceRow (x:xs) newPiece p = (x:(replaceRow xs newPiece (p-1)))

-- Recibe un par de coordenadas, un tablero y una pieza.
-- Coloca dicha pieza en las coordenadas del tablero dado.
replace :: Coordinate -> OnitamaTable -> OnitamaPiece -> OnitamaTable
replace (x,y) table newPiece = 
    [if (y==yAux) then (replaceRow (table !! y) newPiece x) else (table!!yAux) | yAux <- [0..4]]

-- Recibe el tablero y una accion.
-- Retorna el tablero resultado tras la aplicacion de la accion.
doAction :: OnitamaTable -> OnitamaAction -> OnitamaTable
doAction table (Action pieceToMove cardToMove (fx,fy) (tx,ty)) =
    replace (fx,fy) (replace (tx,ty) table (table !! fy !! fx)) NoPiece

-- Recibe una dupla de cartas y una tercer carta.
-- Retorna si la tercer carta es alguna de la dupla.
isACardIn :: [OnitamaCard] -> OnitamaCard -> Bool
isACardIn [c1,c2] c = c==c1 || c ==c2

-- Recibe las cartas, un jugador.
-- Retorna sus cartas.
handOf :: [OnitamaCard] -> OnitamaPlayer -> [OnitamaCard]
handOf [c1,c2,c3,c4,c5] RedPlayer = [c1,c2]
handOf [c1,c2,c3,c4,c5] BluePlayer = [c3,c4]

-- Recibe un estado del juego, un jugador y una accion.
-- Retorna el estado del juego tras la aplicacion de la accion.
-- Revisa:
-- Que sea el turno del jugador.
-- Que la pieza a mover esta en el lugar indicado en el tablero.
-- Que el dueño de la pieza sea el jugador a mover.
-- Que el dstino del movimiento este vacio o ocupado por el enemigo.
-- Que el jugador posea la carta de la accion.
-- Que el juego no ha terminado.
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
next (GameState player cards table ) playerMover (Action pieceToMove cardToMove from to)
    | (player /= playerMover) = error "No es el turno del jugador."
    | ((pieceAt from table) /= pieceToMove) = error "Pieza incorrecta en el tablero."
    | (owner pieceToMove) /= playerMover = error "Esa pieza no es tuya."
    | (not (isEmpty to table || owner (pieceAt to table) == otherPlayer player)) = error "Destino ocupado."
    | endedGame table = error ("El juego ha terminado.")
    | (not (isACardIn (handOf cards player) cardToMove)) = error "El jugador no posee esa carta."
    | otherwise = (GameState (otherPlayer player) (nextCards cards cardToMove) (doAction table (Action pieceToMove cardToMove from to)))

-- Recibe el estado del juego.
-- Retorna el resultado del juego para cada jugador.
-- Si el juego no está terminado, se debe retornar una lista vacía.
result :: OnitamaGame -> [GameResult OnitamaPlayer]
result (GameState player _ table)
    | not (endedGame table) = []
    | otherwise = [Loser player, Winner (otherPlayer player)]

-- Recibe el estado del juego.
-- Retorna un texto representativo que puede ser impreso en la consola.
showGame :: OnitamaGame -> String
showGame (GameState player card table) = show player ++"\n"++ show card ++"\n"++ show table

showGameTest = showGame (beginning [Tiger, Crab, Monkey, Crane, Dragon])

-- Convierte una acción a un texto que puede ser impreso en la consola para mostrarla.
showAction :: OnitamaAction -> String
showAction (Action piece card cor1 cor2) = show piece ++"\n"++ show card ++"\n"++ show cor1 ++"\n"++ show cor2

-- Obtiene una acción a partir de un texto que puede haber sido introducido por el usuario en la consola.
readAction :: String -> OnitamaAction
readAction input = (read input)

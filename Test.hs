module Test where 

import GameFunctions

actionsTest1 = (actions (beginning [Tiger, Tiger, Tiger, Tiger, Tiger])) ==[(RedPlayer,[
    Action (Apprentice RedPlayer) Tiger (0,0) (2,0),
    Action (Apprentice RedPlayer) Tiger (0,0) (2,0),
    Action (Apprentice RedPlayer) Tiger (0,1) (2,1),
    Action (Apprentice RedPlayer) Tiger (0,1) (2,1),
    Action (Master RedPlayer) Tiger (0,2) (2,2),
    Action (Master RedPlayer) Tiger (0,2) (2,2),
    Action (Apprentice RedPlayer) Tiger (0,3) (2,3),
    Action (Apprentice RedPlayer) Tiger (0,3) (2,3),
    Action (Apprentice RedPlayer) Tiger (0,4) (2,4),
    Action (Apprentice RedPlayer) Tiger (0,4) (2,4)
    ]),(BluePlayer,[])]

actionsTest2 = (actions (GameState RedPlayer [Tiger,Tiger,Tiger,Tiger,Tiger] table)) == [(RedPlayer,[
    Action (Apprentice RedPlayer) Tiger (0,0) (2,0),
    Action (Apprentice RedPlayer) Tiger (0,0) (2,0),
    Action (Apprentice RedPlayer) Tiger (0,1) (2,1),
    Action (Apprentice RedPlayer) Tiger (0,1) (2,1),
    Action (Master RedPlayer) Tiger (0,2) (2,2),
    Action (Master RedPlayer) Tiger (0,2) (2,2),
    Action (Apprentice RedPlayer) Tiger (0,3) (2,3),
    Action (Apprentice RedPlayer) Tiger (0,3) (2,3),
    Action (Apprentice RedPlayer) Tiger (0,4) (2,4),
    Action (Apprentice RedPlayer) Tiger (0,4) (2,4)
    ]),(BluePlayer,[])]
    
    where table = [
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Master RedPlayer,        NoPiece,    Master BluePlayer,        NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece]]

nextTest = (next
    (beginning [Tiger, Tiger, Tiger, Tiger, Tiger]) 
    (RedPlayer) 
    (Action (Master RedPlayer) Tiger (0,2) (2,2))) == 
    (GameState BluePlayer [Tiger,Tiger,Tiger,Tiger,Tiger] [
    [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
    [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
    [NoPiece,NoPiece,Master RedPlayer,NoPiece,Master BluePlayer],
    [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
    [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer]])

otherPlayerTest = otherPlayer RedPlayer == BluePlayer && otherPlayer BluePlayer == RedPlayer

playerCardsTest = playerCards RedPlayer [Tiger, Dragon, Crab, Elephant, Frog] == [Tiger, Dragon] 
    && playerCards BluePlayer [Tiger, Dragon, Crab, Elephant, Frog] == [Crab, Elephant] 

table = [
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Master RedPlayer,        NoPiece,    Master BluePlayer,        NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece]]

isEmptyTest = isEmpty (0,0) table == False && isEmpty (1,1) table == True 

moveTest = redMove (0,0) (1,1) == (1,1) && blueMove (3,4) (2,2) == (1,2)

ownerTest = isOwnerOf RedPlayer (Master RedPlayer) && isOwnerOf BluePlayer (Apprentice BluePlayer)

isEnemyPieceTest = isEnemyPiece RedPlayer (0,0) table == False && isEnemyPiece BluePlayer (0,0) table == True

resultTest = result (GameState RedPlayer [Tiger,Frog,Goose,Eel,Elephant] table) == [] &&
    result (GameState RedPlayer [Tiger,Frog,Goose,Eel,Elephant] [
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [NoPiece             ,    NoPiece,    Master BluePlayer,        NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece],
            [Apprentice RedPlayer,    NoPiece,    Apprentice BluePlayer,    NoPiece,    NoPiece]]) == [Loser RedPlayer, Winner BluePlayer]



-- showGameTest = showGame (beginning [Tiger, Crab, Monkey, Crane, Dragon]) == "RedPlayer\n[Tiger,Crab,Monkey,Crane,Dragon] \n [
--     [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
--     [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
--     [Master RedPlayer,NoPiece,NoPiece,NoPiece,Master BluePlayer],
--     [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer],
--     [Apprentice RedPlayer,NoPiece,NoPiece,NoPiece,Apprentice BluePlayer]]"

showActionTest = showAction (head (possibleActions (GameState RedPlayer [Tiger,Goose,Boar,Eel,Elephant] table))) == "Apprentice RedPlayer\nTiger\n(0,0)\n(2,0)"

readActionTest = readAction "Action (Master BluePlayer) Tiger (0,0) (2,0)" == Action (Master BluePlayer) Tiger (0,0) (2,0)

handOfTest = handOf [Dragon,Tiger,Frog,Rabbit,Boar] RedPlayer == [Dragon,Tiger] && handOf [Dragon,Tiger,Frog,Rabbit,Boar] BluePlayer == [Frog,Rabbit] 

isACardInTest = isACardIn [Dragon,Tiger] Dragon == True && isACardIn [Dragon,Tiger] Tiger == True && isACardIn [Dragon,Tiger] Frog == False

isMasterTest = if (isMaster (Master RedPlayer) == True && isMaster (Master BluePlayer) == True) then True else False 

test = nextTest && actionsTest1 && actionsTest2 && otherPlayerTest && playerCardsTest && 
    isEmptyTest && moveTest && ownerTest && isEnemyPieceTest && resultTest && showActionTest && readActionTest && handOfTest && isACardInTest && isMasterTest
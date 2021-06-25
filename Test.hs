module Test where 

import Juego

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

test = nextTest && actionsTest1 && actionsTest2
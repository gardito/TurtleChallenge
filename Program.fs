open System

// turtle: direction / point
// mine: point
// exit: point
// board: 2 sizes (x and y)
// moves: list of 'r' and 'm'
// 3 different states: Still in Danger, Mine Hit, Out of Bound and Success

type Point = {
    X: int
    Y: int
}

type Direction = N | E | S | W

type Turtle = {
    Position: Point
    Direction: Direction
}

type Mine = Point

type Exit = Point

type Movement =
    | Move
    | Rotate

type TurtleState =
    | StillInDanger
    | Success
    | MineHit
    | OutOfBound

type Board = {
    sizeX: int
    sizeY: int
}

type RotateTurtle = Turtle -> Turtle
type MoveTurtle =  Turtle -> Turtle

let rotateTurtle : RotateTurtle = 
    fun turtle -> 
        match turtle.Direction with
        | N -> { turtle with Direction = E }
        | E -> { turtle with Direction = S }
        | S -> { turtle with Direction = W }
        | W -> { turtle with Direction = N }

let moveTurtle : MoveTurtle =
    fun turtle -> 
        match turtle.Direction with
        | N -> { turtle with Position = { X = turtle.Position.X; Y = turtle.Position.Y + 1 } }
        | E -> { turtle with Position = { X = turtle.Position.X + 1; Y = turtle.Position.Y } }
        | S -> { turtle with Position = { X = turtle.Position.X; Y = turtle.Position.Y - 1 } }
        | W -> { turtle with Position = { X = turtle.Position.X - 1; Y = turtle.Position.Y } }

let rec executeTurtle (moveTurtle:MoveTurtle) (rotateTurtle:RotateTurtle) (turtle:Turtle) (movements:Movement list) (mines:Mine list) (exit:Exit) : TurtleState = 
    if movements.IsEmpty then StillInDanger
    elif List.contains turtle.Position mines then MineHit
    elif turtle.Position = exit then Success
    else 
        let newTurtle =
            match movements.Head with
            | Move -> moveTurtle turtle
            | Rotate -> rotateTurtle turtle
        executeTurtle moveTurtle rotateTurtle newTurtle movements.Tail mines exit

[<EntryPoint>]
let main argv =
    let board = { sizeX = 10; sizeY = 10 }
    let turtle = { Position = { X = 1; Y = 1 }; Direction = N}
    let movements = [Move; Move; Rotate; Rotate; Move]
    let mines = [{ X = 4; Y = 2 }]
    let exit = { X = 1; Y = 2 }

    let handleMovements = executeTurtle moveTurtle rotateTurtle

    let result = handleMovements turtle movements mines exit

    match result with
        | StillInDanger -> printfn "Turtle is still in danger!"
        | MineHit -> printfn "Turtle...Mine!"
        | Success -> printfn "Turtle...Success!"
        | _ -> printfn "wait, what?"

    0 // return an integer exit code

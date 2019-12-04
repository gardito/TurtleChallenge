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

type Direction = | N | E | S | W

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

type RotateTurtle = Turtle -> Direction -> Turtle
type MoveTurtle =  Turtle -> Turtle
type CheckState = Point -> Mine -> Exit -> TurtleState

let rotateTurtle : RotateTurtle = 
    fun turtle direction -> 
        match direction with
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

let checkState : CheckState =
    fun turtlePos mine exit -> 
        if turtlePos = mine then MineHit
        elif turtlePos = exit then Success
        else StillInDanger

[<EntryPoint>]
let main argv =
    let board = { sizeX = 10; sizeY = 10 }
    let turtle = { Position = { X = 1; Y = 1 }; Direction = N}

    0 // return an integer exit code

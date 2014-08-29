﻿// Type abbreviations
type label = string
type identifier = string
type index = int
type MethodInfo = System.Reflection.MethodInfo

/// Small Basic arithmetic operation
type arithmetic = Add | Subtract | Multiply | Divide
/// Small Basic comparison operaton
type comparison = Eq | Ne | Lt | Gt | Le | Ge
/// Small Basic logical operation
type logical = And | Or
/// Small Basic value
type value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
/// Small Basic expression
type expr =
    | Literal of value
    | Var of identifier
    | GetAt of location
    | Func of call
    | Neg of expr
    | Arithmetic of expr * arithmetic * expr
    | Comparison of expr * comparison * expr
    | Logical of expr * logical * expr
and location =
    | Location of identifier * expr[]
and call =
    | Call of MethodInfo * expr[]
/// Assignment
type assign =
    | Set of identifier * expr
/// Small Basic instruction
type instruction =
    | Assign of assign
    | SetAt of location * expr 
    | Action of call
    | For of assign * expr * expr
    | EndFor
    | If of expr
    | Else
    | EndIf
    | While of expr
    | EndWhile
    | Sub of identifier
    | EndSub
    | GoSub of identifier
    | Label of label
    | Goto of label

/// Converts value to obj
let fromObj (x:obj) =
    match x with
    | :? bool as x -> Bool x
    | :? int as x -> Int x
    | :? double as x -> Double x
    | :? string as x -> String x
    | null -> Int 0
    | x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to obj
let toObj = function
    | Bool x -> box x
    | Int x -> box x
    | Double x -> box x
    | String x -> box x
/// Converts value to int
let toInt = function
    | Bool x -> raise (new System.NotSupportedException())
    | Int x -> x
    | Double x -> int x
    | String x -> int x
/// Converts value to bool
let toBool = function
    | Bool x -> x
    | _ -> raise (new System.NotSupportedException())
/// Coerces a tuple of numeric values to double
let (|AsDoubles|_|) = function
    | Double l, Double r -> Some(l,r)
    | Int l, Double r -> Some(double l,r)
    | Double l, Int r -> Some(l,double r)
    | _, _ -> None
/// Compares values
let compare lhs rhs =
    match lhs, rhs with
    | Bool l, Bool r -> l.CompareTo(r)
    | Int l, Int r -> l.CompareTo(r)
    | AsDoubles (l,r) -> l.CompareTo(r)
    | String l, String r -> l.CompareTo(r)
    | _ -> raise (new System.NotSupportedException(sprintf "%A %A" lhs rhs))

open System.Collections.Generic

type VarLookup = Dictionary<identifier,value>
type ArrayLookup = Dictionary<identifier,Dictionary<value,value>>

/// Evaluates expressions
let rec eval state (expr:expr) =
    let (vars:VarLookup), (arrays:ArrayLookup) = state
    match expr with
    | Literal x -> x
    | Var identifier -> vars.[identifier]
    | GetAt(Location(identifier,[|index|])) -> arrays.[identifier].[eval state index]
    | GetAt(Location(identifier,_)) -> raise (System.NotSupportedException())
    | Func(call) -> invoke state call
    | Neg x -> arithmetic (eval state x) Multiply (Int(-1))
    | Arithmetic(l,op,r) -> arithmetic (eval state l) op (eval state r)
    | Comparison(l,op,r) -> comparison (eval state l) op (eval state r)
    | Logical(l,op,r) -> logical (eval state l) op (eval state r)
and comparison lhs op rhs =
    let x = compare lhs rhs
    match op with
    | Eq -> x = 0   | Ne -> x <> 0
    | Lt -> x < 0   | Gt -> x > 0
    | Le -> x <= 0  | Ge -> x >= 0
    |> fromObj
and arithmetic lhs op rhs =
    match op, (lhs, rhs) with
    | Add, (Int l,Int r) -> Int(l + r)
    | Add, AsDoubles (l,r) -> Double(l + r)
    | Add, (String l, String r) -> String(l + r)
    | Subtract, (Int l,Int r) -> Int(l - r)
    | Subtract, AsDoubles (l,r) -> Double(l - r)
    | Multiply, (Int l,Int r) -> Int(l * r)
    | Multiply, AsDoubles (l,r) -> Double(l * r)
    | Divide, (Int l,Int r) -> Int(l - r)
    | Divide, AsDoubles (l,r) -> Double(l - r)
    | _ -> raise (System.NotImplementedException())
and logical lhs op rhs =
    match op, lhs, rhs with
    | And, Bool l, Bool r -> Bool(l && r)
    | Or, Bool l, Bool r -> Bool(l || r)
    | _, _, _ -> raise (System.NotImplementedException())
and invoke state (Call(mi,args)) =
    let args = args |> Array.map (eval state >> toObj)
    mi.Invoke(null, args) |> fromObj

/// Runs program
let run (program:instruction[]) =
    /// Program index
    let pi = ref 0
    /// Variable lookup   
    let variables = VarLookup()
    /// Array lookup
    let arrays = ArrayLookup()
    /// Current state
    let state = variables, arrays
    /// For from EndFor lookup
    let forLoops = Dictionary<index, index * identifier * expr * expr>()
    /// While from EndWhile lookup
    let whileLoops = Dictionary<index, index>()
    /// Call stack for Gosubs
    let callStack = Stack<index>()
    /// Evaluates expression with variables
    let eval = eval (variables,arrays)
    /// Assigns result of expression to variable
    let assign (Set(identifier,expr)) = variables.[identifier] <- eval expr
    /// Finds first index of instructions
    let findFirstIndex start (inc,dec) instructions =
        let mutable i = start
        let mutable nest = 0
        while nest > 0 || instructions |> List.exists ((=) program.[i]) |> not do 
            if inc program.[i] then nest <- nest + 1
            if nest > 0 && dec program.[i] then nest <- nest - 1
            i <- i + 1
        i
    /// Finds index of instruction
    let findIndex start (inc,dec) instruction =
        findFirstIndex start (inc,dec) [instruction]
    let isIf = function If(_) -> true | _ -> false
    let isEndIf = (=) EndIf
    let isFor = function For(_,_,_) -> true | _ -> false
    let isEndFor = (=) EndFor
    let isWhile = function While(_) -> true | _ -> false
    let isEndWhile = (=) EndWhile
    let isFalse _ = false
    /// Instruction step
    let step () =
        let instruction = program.[!pi]
        match instruction with
        | Assign(set) -> assign set
        | SetAt(Location(identifier,[|index|]),expr) ->
            let array = 
                match arrays.TryGetValue(identifier) with
                | true, array -> array
                | false, _ -> 
                    let array = Dictionary<value,value>()
                    arrays.Add(identifier,array)
                    array
            array.[eval index] <- eval expr
        | SetAt(Location(_,_),expr) -> raise (System.NotSupportedException())
        | Action(call) -> invoke state call |> ignore
        | If(condition) ->            
            if eval condition |> toBool |> not then
                let index = findFirstIndex (!pi+1) (isIf, isEndIf) [Else;EndIf]
                pi := index
        | Else ->
            let index = findIndex !pi (isIf,isEndIf) EndIf
            pi := index
        | EndIf -> ()
        | For((Set(identifier,expr) as from), target, step) ->
            assign from
            let index = findIndex (!pi+1) (isFor,isEndFor) EndFor
            forLoops.[index] <- (!pi, identifier, target, step)
            if toInt(variables.[identifier]) > toInt(eval target) 
            then pi := index
        | EndFor ->
            let start, identifier, target, step = forLoops.[!pi]
            let x = variables.[identifier]
            variables.[identifier] <- arithmetic x Add (eval step)
            if toInt(variables.[identifier]) <= toInt(eval target) 
            then pi := start
        | While condition ->
            let index = findIndex (!pi+1) (isWhile,isEndWhile) EndWhile
            whileLoops.[index] <- !pi 
            if eval condition |> toBool |> not then pi := index
        | EndWhile ->
            pi := whileLoops.[!pi] - 1
        | Sub(identifier) ->
            pi := findIndex (!pi+1) (isFalse, isFalse) EndSub
        | GoSub(identifier) ->
            let index = findIndex 0 (isFalse, isFalse) (Sub(identifier))
            callStack.Push(!pi)
            pi := index
        | EndSub ->
            pi := callStack.Pop()
        | Label(label) -> ()
        | Goto(label) -> pi := findIndex 0 (isFalse,isFalse) (Label(label))
    while !pi < program.Length do step (); incr pi

// Embedded DSL

let B x = Literal(Bool(x))
let I x = Literal(Int(x))
let D x = Literal(Double(x))
let S x = Literal(String(x))
let AT(name,expr) = Location(name,[|expr|])
let (.<-) (name:string) (expr:expr) = Assign(Set(name, expr))
let (.<!-) (location:location) (expr:expr) = SetAt(location,expr)
let (!) (name:string) = Var(name)
let (!!) (location:location) = GetAt(location)
let (.+) (lhs:expr) (rhs:expr) = Arithmetic(lhs, Add, rhs)
let (.-) (lhs:expr) (rhs:expr) = Arithmetic(lhs, Subtract, rhs)
let (.*) (lhs:expr) (rhs:expr) = Arithmetic(lhs, Multiply, rhs)
let (./) (lhs:expr) (rhs:expr) = Arithmetic(lhs, Divide, rhs)
let (.=) (lhs:expr) (rhs:expr) = Comparison(lhs, Eq, rhs)
let (.<>) (lhs:expr) (rhs:expr) = Comparison(lhs, Ne, rhs)
let (.<) (lhs:expr) (rhs:expr) = Comparison(lhs, Lt, rhs)
let (.<=) (lhs:expr) (rhs:expr) = Comparison(lhs, Le, rhs)
let (.>) (lhs:expr) (rhs:expr) = Comparison(lhs, Gt, rhs)
let (.>=) (lhs:expr) (rhs:expr) = Comparison(lhs, Ge, rhs)
let AND lhs rhs = Logical(lhs,And,rhs)
let OR lhs rhs = Logical(lhs,And,rhs)
let IF(condition:expr) = If(condition)
let ELSE = Else
let ENDIF = EndIf
let FOR(var:identifier, from:expr, ``to``:expr) = For(Set(var, from), ``to``, I(1))
let ENDFOR = EndFor
let WHILE(condition) = While(condition)
let ENDWHILE = EndWhile
let SUB(name) = Sub(name)
let ENDSUB = EndSub
let GOSUB(name) = GoSub(name)
let GOTO(label) = Goto(label)
let LABEL(label) = Label(label)

let PRINT x = 
    let writeLine = typeof<System.Console>.GetMethod("WriteLine",[|typeof<obj>|])
    Action(Call(writeLine, [|x|]))

/// Small Basic program
let program =
    [|
        SUB("Modulus")
        "Result" .<- !"Dividend"
        WHILE( !"Result" .>= !"Divisor")
        "Result" .<- !"Result" .- !"Divisor"
        ENDWHILE
        ENDSUB

        FOR("A", I(1), I(100))
        
        "Dividend" .<- !"A"
        "Divisor" .<- I(15)
        GOSUB("Modulus")
        IF(!"Result" .= I(0))
        PRINT(S"FizzBuzz")
        ELSE
        
        "Dividend" .<- !"A"
        "Divisor" .<- I(3)
        GOSUB("Modulus")
        IF(!"Result" .= I(0))
        PRINT(S"Fizz")
        ELSE

        "Dividend" .<- !"A"
        "Divisor" .<- I(5)
        GOSUB("Modulus")
        IF(!"Result" .= I(0))
        PRINT(S"Buzz")
        ELSE

        PRINT(!"A")
        
        ENDIF
        ENDIF
        ENDIF
        ENDFOR
    |]

run program
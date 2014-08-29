﻿#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"

// [snippet:Abstract Syntax Tree]
// Type abbreviations
type label = string
type identifier = string
type index = int
type Hashtable<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
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
    | Array of Hashtable<value,value>
/// Small Basic expression
type expr =
    | Literal of value
    | Var of identifier
    | GetAt of location
    | Func of invoke
    | Neg of expr
    | Arithmetic of expr * arithmetic * expr
    | Comparison of expr * comparison * expr
    | Logical of expr * logical * expr
and location =
    | Location of identifier * expr list
and invoke =
    | Method of string * string * expr[]
    | PropertyGet of string * string
type assign =
    | Set of identifier * expr
/// Small Basic instruction
type instruction =
    | Assign of assign
    | SetAt of location * expr
    | PropertySet of string * string * expr
    | Action of invoke
    | For of assign * expr * expr
    | EndFor
    | If of expr
    | ElseIf of expr
    | Else
    | EndIf
    | While of expr
    | EndWhile
    | Sub of identifier
    | EndSub
    | GoSub of identifier
    | Label of label
    | Goto of label
// [/snippet]

// [snippet:Parser]
open FParsec

let pnumliteral: Parser<expr, unit> =
    let numberFormat = NumberLiteralOptions.AllowFraction
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Literal(Int (int nl.String))
            else Literal(Double (float nl.String))

let ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c='\r') // spaces
let str_ws s = pstring s .>> ws
let str_ws1 s = pstring s .>> spaces1

let pstringliteral = 
    between (pstring "\"") (pstring "\"") (manySatisfy (fun x -> x <> '"')) 
    |>> (fun s -> Literal(String(s)))

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
let pidentifier_ws = pidentifier .>> ws
let pvar = pidentifier |>> (fun x -> Var(x))

let pinvoke, pinvokeimpl = createParserForwardedToRef ()
let pfunc = pinvoke |>> (fun x -> Func(x))

let plocation, plocationimpl = createParserForwardedToRef ()
let pgetat = plocation |>> (fun loc -> GetAt(loc))

let pvalue = 
    choice [
        pnumliteral; pstringliteral
        attempt pgetat <|> attempt pfunc <|> attempt pvar
    ]

type Assoc = Associativity

let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
let parithmetic = oppa.ExpressionParser
let terma = (pvalue .>> ws) <|> between (str_ws "(") (str_ws ")") parithmetic
oppa.TermParser <- terma
oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
oppa.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
oppa.AddOperator(PrefixOperator("-", ws, 2, true, fun x -> Neg(x)))

let oppc = new OperatorPrecedenceParser<expr,unit,unit>()
let pcomparison = oppc.ExpressionParser
let termc = (parithmetic .>> ws) <|> between (str_ws "(") (str_ws ")") pcomparison
oppc.TermParser <- termc
oppc.AddOperator(InfixOperator("=", ws, 1, Assoc.Left, fun x y -> Comparison(x, Eq, y)))
oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, Ne, y)))
oppc.AddOperator(InfixOperator("<=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Le, y)))
oppc.AddOperator(InfixOperator(">=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Ge, y)))
oppc.AddOperator(InfixOperator("<", ws, 2, Assoc.Left, fun x y -> Comparison(x, Lt, y)))
oppc.AddOperator(InfixOperator(">", ws, 2, Assoc.Left, fun x y -> Comparison(x, Gt, y)))

let oppl = new OperatorPrecedenceParser<expr,unit,unit>()
let plogical = oppl.ExpressionParser
let terml = (pcomparison .>> ws) <|> between (str_ws "(") (str_ws ")") plogical
oppl.TermParser <- terml
oppl.AddOperator(InfixOperator("And", ws, 1, Assoc.Left, fun x y -> Logical(x,And,y)))
oppl.AddOperator(InfixOperator("Or", ws, 1, Assoc.Left, fun x y -> Logical(x,Or,y)))

let pmember = pipe3 (pidentifier_ws) (pchar '.') (pidentifier_ws) (fun tn _ mn -> tn,mn) 
let ptuple = between (str_ws "(") (str_ws ")") (sepBy parithmetic (str_ws ","))
pinvokeimpl := 
    pipe2 pmember (opt ptuple)
        (fun (tn,mn) args -> 
        match args with
        | Some args -> Method(tn, mn, args |> List.toArray)
        | None -> PropertyGet(tn,mn)
        )

let paction = pinvoke |>> (fun x -> Action(x))
let pset = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Set(id, e))
let passign = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Assign(Set(id, e)))
let ppropertyset = pipe3 pmember (str_ws "=") parithmetic (fun (tn,pn) _ e -> PropertySet(tn,pn,e))

let pindex = str_ws "[" >>. parithmetic .>> str_ws "]"
let pindices = many1 pindex
plocationimpl := pipe2 pidentifier_ws pindices (fun id xs -> Location(id,xs))
let psetat = pipe3 plocation (str_ws "=") parithmetic (fun loc _ e -> SetAt(loc, e))

let pfor =
    let pfrom = str_ws1 "For" >>. pset
    let pto = str_ws1 "To" >>. parithmetic
    let pstep = str_ws1 "Step" >>. parithmetic
    let toStep = function None -> Literal(Int(1)) | Some s -> s
    pipe3 pfrom pto (opt pstep) (fun f t s -> For(f, t, toStep s))
let pendfor = str_ws "EndFor" |>> (fun _ -> EndFor)

let pwhile = str_ws1 "While" >>. plogical |>> (fun e -> While(e))
let pendwhile = str_ws "EndWhile" |>> (fun _ -> EndWhile)

let pif = str_ws1 "If" >>. plogical .>> str_ws "Then" |>> (fun e -> If(e))
let pelseif = str_ws1 "ElseIf" >>. pcomparison .>> str_ws "Then" |>> (fun e -> ElseIf(e))
let pelse = str_ws "Else" |>> (fun _ -> Else)
let pendif = str_ws "EndIf" |>> (fun _ -> EndIf)

let psub = str_ws1 "Sub" >>. pidentifier |>> (fun name -> Sub(name))
let pendsub = str_ws "EndSub" |>> (fun _ -> EndSub)
let pgosub = pidentifier_ws .>> str_ws "()" |>> (fun routine -> GoSub(routine))

let plabel = pidentifier_ws .>> str_ws ":" |>> (fun label -> Label(label))
let pgoto = str_ws1 "Goto" >>. pidentifier |>> (fun label -> Goto(label))

let pinstruct = 
    [
        pfor;pendfor
        pwhile;pendwhile
        pif; pelseif; pelse; pendif
        psub; pendsub; pgosub                 
        ppropertyset; passign; psetat
        paction
        plabel; pgoto
    ]
    |> List.map attempt
    |> choice

type Line = Blank | Instruction of instruction
let pcomment = pchar '\'' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
let peol = pcomment <|> (pchar '\n')
let pinstruction = ws >>. pinstruct .>> peol |>> (fun i -> Instruction i)
let pblank = ws >>. peol |>> (fun _ -> Blank)
let plines = many (pinstruction <|> pblank) .>> eof
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction i -> Some i | Blank -> None) 
        |> List.toArray
    | Failure(errorMsg, e, s) -> failwith errorMsg
// [/snippet]

type Color = System.ConsoleColor
// [snippet:Library]
type TextWindow private () =
    static member WriteLine (o:obj) = System.Console.WriteLine(o)
    static member ForegroundColor
        with get () = System.Console.ForegroundColor.ToString()
        and set color =   
            let color = Color.Parse(typeof<Color>, color, true)
            System.Console.ForegroundColor <- color :?> Color
type Clock private () =
    static let now() = System.DateTime.Now
    static member Year = now().Year
    static member Month = now().Month
    static member Day = now().Day

type IMarker = interface end
let getLibraryType name = typeof<IMarker>.DeclaringType.GetNestedType(name) 
// [/snippet]

// [snippet:Interpreter]
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
    | Array x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to int
let toInt = function
    | Bool x -> raise (new System.NotSupportedException())
    | Int x -> x
    | Double x -> int x
    | String x -> int x
    | Array x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to bool
let toBool = function
    | Bool x -> x
    | _ -> raise (new System.NotSupportedException())
/// Converts value to array
let toArray = function
    | Array x -> x
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

/// Evaluates expressions
let rec eval state (expr:expr) =
    let (vars:VarLookup) = state
    match expr with
    | Literal x -> x
    | Var identifier -> vars.[identifier]
    | GetAt(Location(identifier,[index])) -> 
        let array = vars.[identifier] |> toArray
        array.[eval state index]
    | GetAt(Location(identifier,xs)) -> raise (System.NotSupportedException())
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
    | _, _, _ -> raise (System.NotSupportedException())
and invoke state invoke =
    match invoke with
    | Method(tn, name, args) ->
        let t = getLibraryType tn
        let mi = t.GetMethod(name)
        let args = args |> Array.map (eval state >> toObj)
        mi.Invoke(null, args) |> fromObj
    | PropertyGet(tn, name) ->
        let t = getLibraryType tn
        let pi = t.GetProperty(name)        
        pi.GetValue(null, [||]) |> fromObj

/// Runs program
let run (program:instruction[]) =
    /// Program index
    let pi = ref 0
    /// Variable lookup   
    let variables = VarLookup()   
    /// For from EndFor lookup
    let forLoops = Dictionary<index, index * identifier * expr * expr>()
    /// While from EndWhile lookup
    let whileLoops = Dictionary<index, index>()
    /// Call stack for Gosubs
    let callStack = Stack<index>()
    /// Evaluates expression with variables
    let eval = eval variables
    /// Assigns variable with result of expression
    let assign (Set(identifier,expr)) = variables.[identifier] <- eval expr    
    /// Sets property with result of expression
    let propertySet(tn,pn,expr) =
        let t = getLibraryType tn
        let pi = t.GetProperty(pn)
        pi.SetValue(null, eval expr |> toObj, [||])
    /// Obtains an array for the specified identifier
    let obtainArray identifier =
        match variables.TryGetValue(identifier) with
        | true, Array(array) -> array
        | true, _ -> raise (System.NotSupportedException())
        | false, _ -> 
            let array = Hashtable<value,value>()
            variables.Add(identifier,Array(array))
            array
    /// Sets array value at index with result of expression
    let setAt(identifier,index,expr) =        
        let array = obtainArray identifier
        array.[eval index] <- eval expr
    /// Finds first index of instructions
    let findFirstIndex start (inc,dec) isMatch =
        let mutable i = start
        let mutable nest = 0
        while nest > 0 || isMatch program.[i] |> not do 
            if inc program.[i] then nest <- nest + 1
            if nest > 0 && dec program.[i] then nest <- nest - 1
            i <- i + 1
        i
    /// Finds index of instruction
    let findIndex start (inc,dec) instruction =
        findFirstIndex start (inc,dec) ((=) instruction)
    let isIf = function If(_) -> true | _ -> false
    let isElseIf = function ElseIf(_) -> true | _ -> false
    let isElse = (=) Else
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
        | PropertySet(tn,pn,expr) -> propertySet(tn,pn,expr)           
        | SetAt(Location(identifier,[index]),expr) -> setAt(identifier,index,expr)
        | SetAt(_) -> raise (System.NotImplementedException())
        | Action(call) -> invoke variables call |> ignore
        | If(condition) | ElseIf(condition) ->           
            if eval condition |> toBool |> not then
                let isMatch x = isElseIf x || isElse x || isEndIf x
                let index = findFirstIndex (!pi+1) (isIf, isEndIf) isMatch
                pi := 
                    if program.[index] |> isElseIf 
                    then index - 1
                    else index         
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
// [/snippet]

// [snippet:Fizz Buzz sample]
let source = """
' Sets Result to Modulus
Sub Modulus
  Result = Dividend
  While Result >= Divisor
    Result = Result - Divisor
  EndWhile
EndSub

For A = 1 To 100 ' Print from 1 to 100
  Dividend = A
  Divisor = 3
  Modulus()
  Mod3 = Result ' A % 3
  Divisor = 5
  Modulus()
  Mod5 = Result ' A % 5
  If Mod3 = 0 And Mod5 = 0 Then
    TextWindow.WriteLine("FizzBuzz")  
  ElseIf Mod3 = 0 Then
    TextWindow.WriteLine("Fizz")
  ElseIf Mod5 = 0 Then
    TextWindow.WriteLine("Buzz")
  Else
    TextWindow.WriteLine(A)        
  EndIf
EndFor
"""

let program = parse source
run program
// [/snippet]
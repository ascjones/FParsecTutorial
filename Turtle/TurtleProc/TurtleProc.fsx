﻿module AST =
   type distance = int
   type degrees = int
   type count = int
   type name = string
   type command =
      | Forward of distance
      | Turn of degrees
      | SetRandomPosition
      | Repeat of count * command list
      | Call of name
      | Proc of name * command list

(*[omit:Windows Forms references]*)
#if INTERACTIVE
#r "System.Drawing.dll"
#r "System.Windows.Forms.dll"
#endif
(*[/omit]*)

module Interpreter =
   open AST
   open System
   open System.Drawing
   open System.Windows.Forms

   type Turtle = { X:float; Y:float; A:int }

   let execute commands =
      let procs = ref Map.empty
      let width, height = 640, 480
      let form = new Form (Text="Turtle", Width=width, Height=height)
      let image = new Bitmap(width, height)
      let picture = new PictureBox(Dock=DockStyle.Fill, Image=image)
      do  form.Controls.Add(picture)
      let turtle = { X=float width/2.0; Y=float height/2.0; A = -90 }
      use pen = new Pen(Color.Red)
      let rand = let r = Random() in fun n -> r.Next(n) |> float
      let drawLine (x1,y1) (x2,y2) =
         use graphics = Graphics.FromImage(image)
         graphics.DrawLine(pen,int x1,int y1,int x2, int y2)
      let rec perform turtle = function
         | Forward n ->
            let r = float turtle.A * Math.PI / 180.0
            let dx, dy = float n * cos r, float n * sin r
            let x, y =  turtle.X, turtle.Y
            let x',y' = x + dx, y + dy
            drawLine (x,y) (x',y')
            { turtle with X = x'; Y = y' }
         | Turn n -> { turtle with A=turtle.A + n }
         | SetRandomPosition -> { turtle with X=rand width; Y=rand height }
         | Repeat(n,commands) ->
            let rec repeat turtle = function
               | 0 -> turtle
               | n -> repeat (performAll turtle commands) (n-1)
            repeat turtle n
         | Proc(name,commands) -> procs := Map.add name commands !procs; turtle
         | Call(name) -> (!procs).[name] |> performAll turtle
      and performAll = List.fold perform
      performAll turtle commands |> ignore
      form.ShowDialog() |> ignore

(*[omit:FParsec references]*)
#if INTERACTIVE
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#endif
(*[/omit]*)

module Parser =

   open AST
   open FParsec

   let procs = ref []

   let pforward = 
      (pstring "forward" <|> pstring "fd") >>. spaces1 >>. pfloat 
      |>> fun x -> Forward(int x)
   let pleft = 
      (pstring "left" <|> pstring "lt") >>. spaces1 >>. pfloat 
      |>> fun x -> Turn(int -x)
   let pright = 
      (pstring "right" <|> pstring "rt") >>. spaces1 >>. pfloat 
      |>> fun x -> Turn(int x)
   let prandom = 
      pstring "set-random-position"
      |>> fun _ -> SetRandomPosition
   let prepeat, prepeatimpl = createParserForwardedToRef ()
   let pcall, pcallimpl = createParserForwardedToRef ()

   let pcommand = pforward <|> pleft <|> pright <|> prandom <|> prepeat <|> pcall

   let updateCalls () =
      pcallimpl := 
         choice [for name in !procs -> pstring name |>> fun _ -> Call(name)]
   updateCalls()

   let block = between (pstring "[" .>> spaces) (pstring "]") 
                       (sepEndBy pcommand spaces1)
   
   prepeatimpl := 
      pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. block
      |>> fun (n,commands) -> Repeat(int n, commands)

   let pidentifier =
      let isIdentifierFirstChar c = isLetter c || c = '-'
      let isIdentifierChar c = isLetter c || isDigit c || c = '-'
      many1Satisfy2L isIdentifierFirstChar isIdentifierChar  "identifier"

   let pheader = pstring "to" >>. spaces1 >>. pidentifier .>> spaces1
   let pbody = many (pcommand .>> spaces1)
   let pfooter = pstring "end"

   let pproc =
      pheader .>>. pbody .>> pfooter
      |>> fun (name,body) -> procs := name::!procs; updateCalls(); Proc(name, body)

   let parser =
      spaces >>. (sepEndBy (pcommand <|> pproc) spaces1)

   let parse code =
      match run parser code with
      | Success(result,_,_) -> result
      | Failure(msg,_,_) -> failwith msg

let code = "
   to square
     repeat 4 [forward 50 right 90]
   end
   to flower
     repeat 36 [right 10 square]
   end
   to garden
     repeat 25 [set-random-position flower]
   end
   garden
   "
let program = Parser.parse code
Interpreter.execute program
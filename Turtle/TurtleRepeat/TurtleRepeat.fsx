﻿module AST =
   type distance = int
   type degrees = int
   type count = int
   type command =
      | Forward of distance
      | Left of degrees
      | Right of degrees
      | Repeat of count * command list

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
      let form = new Form (Text="Small Logo", Width=640, Height=480)
      let width, height = 500, 500
      let image = new Bitmap(width, height)
      let picture = new PictureBox(Dock=DockStyle.Fill, Image=image)
      do  form.Controls.Add(picture)
      let turtle = { X=float width/2.0; Y=float height/2.0; A = -90 }
      let pen = new Pen(Color.Red)
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
         | Left n -> { turtle with A=turtle.A + n }
         | Right n -> { turtle with A=turtle.A - n }
         | Repeat(n,commands) ->
            let rec repeat turtle = function
               | 0 -> turtle
               | n -> repeat (performAll turtle commands) (n-1)
            repeat turtle n
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

   let pforward = 
      (pstring "forward" <|> pstring "fd") >>. spaces1 >>. pfloat 
      |>> fun x -> Forward(int x)
   let pleft = 
      (pstring "left" <|> pstring "lt") >>. spaces1 >>. pfloat 
      |>> fun x -> Left(int x)
   let pright = 
      (pstring "right" <|> pstring "rt") >>. spaces1 >>. pfloat 
      |>> fun x -> Right(int x)

   let prepeat, prepeatimpl = createParserForwardedToRef ()

   let pcommand = pforward <|> pleft <|> pright <|> prepeat

   let block = between (pstring "[") (pstring "]") (many1 (pcommand .>> spaces))

   prepeatimpl := 
      pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. block
      |>> fun (n,commands) -> Repeat(int n, commands)

   let parse code =
      match run (many pcommand) code with
      | Success(result,_,_) -> result
      | Failure(msg,_,_) -> failwith msg

let code = "repeat 10 [right 36 repeat 5 [forward 54 right 72]]"
let program = Parser.parse code
Interpreter.execute program
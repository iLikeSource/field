namespace BasicOperaions

///  Eitherモナド
///  TODO 基本ユーティリティとしてどこかにまとめる
type Either<'T, 'U> = 
    | Left of 'T
    | Right of 'U

module Either =
    type EitherBuilder() =
        member self.Bind (expr, f) = expr |> (function Right x -> f x | x -> x)
        member self.Return (x) = Right x
        member self.ReturnFrom (x) = x
        member self.Delay (f) = f ()

    let either = new EitherBuilder()

type token =
    | Number of float 
    //| Var of string 
    | Plus of float * float
    | Minus of float * float 
    | Multiply of float * float 
    | Division of float * float 
    | Power of float * float

type error = 
    | InvalidExpression     //  数式がおかしい場合

type result = Either<error, string>
      

module Eval =

    open System.Text.RegularExpressions
    open Either

    let private basicNumberNotation    = "\d+\.*\d*"
    let private headNumberNotation     = "^[\+\-]\d+\.*\d*"
    let private numberWithBrNotation   = "\([\+\-]\d+\.*\d*\)"
    let private numberNotation         = basicNumberNotation + "|" + headNumberNotation + "|" + numberWithBrNotation
    let private operatorNotation       = "[\+\-\*\/\^]"
    let private roundBracketsNotation  = "\((?<innerExpr>[^\(^\)]+)\)"
    
    let private numberRegex            = new Regex(numberNotation)       
    let private operatorRegex          = new Regex(operatorNotation)     
    let private roundBracketsRegex     = new Regex(roundBracketsNotation)
    

    let private ( +>> ) x y = x + "\s*" + y
    let private powerExpr    = new Regex(numberNotation +>> "\^" +>> numberNotation)
    let private multiplyExpr = new Regex(numberNotation +>> "\*" +>> numberNotation)
    let private divisionExpr = new Regex(numberNotation +>> "\/" +>> numberNotation)
    let private plusExpr     = new Regex(numberNotation +>> "\+" +>> numberNotation)
    let private minusExpr    = new Regex(numberNotation +>> "\-" +>> numberNotation)
    let private numberExpr   = new Regex(numberNotation)

    
    let private operatorPriority = [ ["^"]; ["*"; "/"]; ["+"; "-"] ]

    /// 
    let private calcOperator = function
        | Plus     (x, y) -> x + y
        | Minus    (x, y) -> x - y
        | Multiply (x, y) -> x * y
        | Division (x, y) -> x / y
        | Power    (x, y) -> x ** y
        | Number   (x)    -> x

    ///  数値から文字列への置換
    ///  負値の場合、()で囲む
    let private floatToString (v:float) = 
        if v >= 0.0 then string v else "(" + string v + ")" 

    let private stringToFloat (s:string) = 
        float <| s.Replace("(", "").Replace(")", "")

    ///  数値を分離する
    let private splitNumbers (s:string) = 
        let ms = numberRegex.Matches(s) 
        //  TODO Seq.cast<Match>で代用できそう
        [| 0 .. ms.Count - 1 |] 
        |> Array.map (fun i -> ms.[i])

    ///  演算子を分離する
    let private splitOperators (s:string) = 
        let numbers = splitNumbers (s)
        let tmpOperators = 
            let ms = operatorRegex.Matches(s)
            [| 0 .. ms.Count - 1 |] 
            |> Array.map (fun i -> ms.[i])
        if numbers.Length = tmpOperators.Length then
            //  同数の場合、行頭演算子を含んでいるため除去
            Array.sub tmpOperators 1 (tmpOperators.Length - 1) 
        else
            tmpOperators 

    ///  最小単位の数式を解釈する
    ///  ex. 1+1, 2*4, 4^1
    let private parseMinimumExpr (s:string) = 
        let numbers   = splitNumbers (s) 
        let operators = splitOperators (s)
        if operators.Length = 1 then
            let n1 = stringToFloat (numbers.[0].Value)
            let n2 = stringToFloat (numbers.[1].Value)
            if      s.Contains "^" then Power (n1, n2)
            else if s.Contains "*" then Multiply (n1, n2)   
            else if s.Contains "/" then Division (n1, n2)   
            else if s.Contains "+" then Plus (n1, n2)   
            else if s.Contains "-" then Minus (n1, n2)   
            else                        failwith "想定外"
        else
            Number (stringToFloat s)
         
    ///  優先する演算子に関する計算を展開する
    let private expandPriorExpr (s:string) = 
        let numbers   = splitNumbers (s) 
        let operators = splitOperators (s)
        if operators.Length = 0 then
            None
        else

            //  最優先の演算子と位置を取得
            let (opIndex, _) =
                operators
                |> Array.mapi (fun i m -> (i, m.Value))
                |> Array.choose (fun (i, targetOp) ->
                    List.tryFindIndex (fun ops ->
                        List.tryFind (fun op -> op = targetOp) ops
                        |> function
                        | Some _ -> true
                        | None   -> false   
                    ) operatorPriority
                    |> function
                    | Some (priority) -> Some (i, priority)
                    | None            -> None 
                )
                |> Array.fold (fun dst src ->
                    let (_, srcPriority) = src
                    let (_, dstPriority) = dst
                    if srcPriority < dstPriority then src else dst
                ) (-1, 100)
       
            let op = operators.[opIndex].Value
        
            //  最優先の演算子と左右の数値を組み合わせる
            let leftNumMatch  = numbers.[opIndex]
            let rightNumMatch = numbers.[opIndex + 1]
            let leftNumIndex  = leftNumMatch.Index  
            let rightNumIndex = rightNumMatch.Index  
            let leftNum       = leftNumMatch.Value
            let rightNum      = rightNumMatch.Value
            let priorExpr     = leftNum + op + rightNum 
        
            //  展開
            let ans = parseMinimumExpr (priorExpr) |> calcOperator |> floatToString

            //  置換先を切り取り計算結果を埋め込む
            let replaced = s.Substring(0, leftNumIndex) + ans + s.Substring(rightNumIndex + rightNum.Length)
            Some (replaced)

    ///  数式を計算する
    ///  ()内は既に展開されている文字列を解く
    let private resolveExpr (s:string) = 
        
        //  再帰的に数式計算を繰り返す
        //  展開する数式がなくなれば終了 
        let rec expandExpr (s:string) = 
            expandPriorExpr (s) |> function
            | None            -> s 
            | Some (expanded) -> expandExpr (expanded)            
        
        s.Trim() 
        |> expandExpr

    ///  ()を処理する
    let rec private processingRoundBrackets (s:string) = 
        let m = roundBracketsRegex.Match(s)
        if m.Success then
            let innerExpr = m.Groups.["innerExpr"].Value
            let left  = s.Substring(0, m.Index)
            let right = s.Substring(m.Index + m.Length)
            let dst = left + "(" + (resolveExpr innerExpr) + ")" + right
            if s = dst then dst else dst |> processingRoundBrackets
        else
            s        
                

    ///  行頭に配置できない演算子が存在する場合はエラーを返す
    let private checkHeadOperator (rlt:result) =
        either {
            let! s = rlt
            if Regex.IsMatch( s, "^[\*\/\^]" ) 
            then return! Left (InvalidExpression)
            else return! rlt
        }
    
    let parse (s:string) =
        either {
            let! expanded =
                Right (s.Trim())
                |> checkHeadOperator
            //! 結果を代入
            return resolveExpr <| processingRoundBrackets (expanded) 
        }



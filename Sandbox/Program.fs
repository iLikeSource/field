// F# の詳細については、http://fsharp.org を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open BasicOperaions
open Either

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    either {
        let! x = Eval.parse ("-5*(4+2)^2")
        return x    
    } |> ignore


    0 // 整数の終了コードを返します

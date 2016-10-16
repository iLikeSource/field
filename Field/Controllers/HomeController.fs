namespace Field.Controllers

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Linq
open System.Web
open System.Web.Mvc
open System.Web.Mvc.Ajax

open BasicOperaions
open Either

type HomeController() =
    inherit Controller()



    ///  TODO いずれHelperに移動する
    let expressionRegex = new Regex("\n*(?<body>(?<indent>\s*)(?<left>.+)\s*=>\s*(?<right>.+)\s*)\n*")
    let formatRegex     = new Regex("%\d*\.\d+[efEF]")

    member this.Index () = this.View()

    member this.Edit () = this.View()
    
    ///  数式処理 
    [<NonAction>]
    member this.ParseExpression (mExpr:Match) = 
        if mExpr.Success then
            let indent = mExpr.Groups.["indent"].Value 
            let left   = mExpr.Groups.["left"].Value
            let right  = mExpr.Groups.["right"].Value
            let result =
                either {
                    let! x = Eval.parse left
                    return x
                } |> function
                | Left (err)     -> "数式エラー"
                | Right (result) -> result
            //  書式はとりあえず未対応 
            //  エスケープ文字追加としてバックスラッシュを行頭に追加
            Some (mExpr.Groups.["body"].Value, indent + left + " => " + result + "\n")        
        else
            None   
    
    ///  数式処理 
    [<NonAction>]
    member this.ParseExpressions (s:string) = 
        expressionRegex.Matches(s) 
        |> Seq.cast<Match>
        |> Seq.choose this.ParseExpression
        |> Seq.fold (fun (dst:string) (input, replacement) -> 
            dst.Replace(input, replacement)
        ) s


    ///  TODO * ... * で囲まれると斜体になるので、HTMLにしてから変換する?
    ///  TODO 複数行だとコード表現が適用されない     
    [<HttpPost>]
    member this.Preview()  =
        this.Request.InputStream.Position <- int64 0
        let reader = new System.IO.StreamReader(this.Request.InputStream)
        let html =
            reader.ReadToEnd()
            |> this.ParseExpressions
            |> FSharp.Markdown.Markdown.TransformHtml
        html + "\n" + Field.Plot.ScatterPlot.t.stub()



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



    ///  TODO ������Helper�Ɉړ�����
    let expressionRegex = new Regex("\n*(?<body>(?<indent>\s*)(?<left>.+)\s*=>\s*(?<right>.+)\s*)\n*")
    let formatRegex     = new Regex("%\d*\.\d+[efEF]")

    member this.Index () = this.View()

    member this.Edit () = this.View()
    
    ///  �������� 
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
                | Left (err)     -> "�����G���["
                | Right (result) -> result
            //  �����͂Ƃ肠�������Ή� 
            //  �G�X�P�[�v�����ǉ��Ƃ��ăo�b�N�X���b�V�����s���ɒǉ�
            Some (mExpr.Groups.["body"].Value, indent + left + " => " + result + "\n")        
        else
            None   
    
    ///  �������� 
    [<NonAction>]
    member this.ParseExpressions (s:string) = 
        expressionRegex.Matches(s) 
        |> Seq.cast<Match>
        |> Seq.choose this.ParseExpression
        |> Seq.fold (fun (dst:string) (input, replacement) -> 
            dst.Replace(input, replacement)
        ) s


    ///  TODO * ... * �ň͂܂��ƎΑ̂ɂȂ�̂ŁAHTML�ɂ��Ă���ϊ�����?
    ///  TODO �����s���ƃR�[�h�\�����K�p����Ȃ�     
    [<HttpPost>]
    member this.Preview()  =
        this.Request.InputStream.Position <- int64 0
        let reader = new System.IO.StreamReader(this.Request.InputStream)
        let html =
            reader.ReadToEnd()
            |> this.ParseExpressions
            |> FSharp.Markdown.Markdown.TransformHtml
        html + "\n" + Field.Plot.ScatterPlot.t.stub()



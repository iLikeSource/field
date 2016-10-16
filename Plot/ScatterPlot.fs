namespace Field.Plot

open Google.DataTable.Net.Wrapper
open XPlot.GoogleCharts

module ScatterPlot =

    let update (rangeMin:float, rangeMax:float) (x) = 
        if      x < rangeMin then (x,        rangeMax)
        else if rangeMax < x then (rangeMin, x       )
        else                      (rangeMin, rangeMax)     

    type t = 
        { dataSource : (float * float) list
          xRange : float * float
          yRange : float * float }
    with
    member this.push (x, y) = 
        { this with 
            dataSource = (x, y) :: this.dataSource;
            xRange     = update this.xRange x;
            yRange     = update this.yRange y }
    member this.pushRange (pairList:(float * float) list) = 
        let (xRange, yRange) = 
            pairList
            |> List.fold (fun (xRange, yRange) (x, y) -> 
                (update xRange x, update yRange y) 
            ) (this.xRange, this.yRange)
        { this with
            dataSource = List.append (List.rev pairList) this.dataSource;
            xRange     = xRange;
            yRange     = yRange }
    member this.draw () = Chart.Line(this.dataSource).Html  
    
    static member empty () = 
        { dataSource = [];
          xRange = (0.0, 0.0);
          yRange = (0.0, 0.0) }      
    static member stub () = t.empty().pushRange([ (0.0, 0.0); (1.0, 1.5); (2.0, -1.5) ]).draw()

      
    
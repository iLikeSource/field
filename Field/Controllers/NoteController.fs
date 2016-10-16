namespace Field.Controllers
open System
open System.Collections.Generic
open System.Linq
open System.Net.Http
open System.Web.Http

/// Retrieves values.
[<RoutePrefix("api/note")>]
type ValuesController() =
    inherit ApiController()

    /// Gets the value with index id.
    [<HttpPost>]
    member x.Preview(text:string)  =
        text

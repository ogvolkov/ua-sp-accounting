module ExchangeRate

open System
open System.Net
open System.Xml.Linq

let getNBUrate (date: DateTime) currencyCode =
    let httpClient = new WebClient()
    let url = String.Format("http://buhgalter911.com/Services/ExchangeRateNBU.asmx/GetRates?date={0:MM/dd/yyyy}", date)
    let response = httpClient.DownloadString url
    
    let getElementsByName elementName (root: XContainer) =
        root.Descendants (XName.Get(elementName))

    let getElementValue elementName (root: XContainer)  =
        root |> getElementsByName elementName |> Seq.head |> fun element -> element.Value
        
    response
        |> XDocument.Parse
        |> getElementsByName "NBU_GRV"
        |> Seq.find (fun xNode -> (getElementValue "CodeLit" xNode) = currencyCode)        
        |> fun node ->    
            let amount = node |> getElementValue "Amount" |> Convert.ToDecimal
            let cost = node |> getElementValue "Exch" |> Convert.ToDecimal

            cost / amount
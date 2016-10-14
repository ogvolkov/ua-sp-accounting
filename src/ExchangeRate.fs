module ExchangeRate

open System
open System.Net
open System.Xml.Linq

let getNBUrate (date: DateTime) currencyCode =
    let httpClient = new WebClient()
    let url = String.Format("https://bank.gov.ua/NBUStatService/v1/statdirectory/exchange?valcode={1}&date={0:yyyyMMdd}", date, currencyCode)
    
    let response = httpClient.DownloadString url

    let getElementsByName elementName (root: XContainer) =
        root.Descendants (XName.Get(elementName))

    let getElementValue elementName (root: XContainer)  =
        root |> getElementsByName elementName |> Seq.head |> fun element -> element.Value
        
    response
        |> XDocument.Parse
        |> getElementsByName "exchange" |> Seq.head
        |> getElementsByName "currency"|> Seq.head
        |> getElementValue "rate"
        |> Convert.ToDecimal
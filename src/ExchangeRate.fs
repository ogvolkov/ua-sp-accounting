module ExchangeRate

open System
open System.Net
open System.Xml.Linq

let getNBUrate (date: DateTime) currencyCode =
    let httpClient = new WebClient()
    // between 04.04.204 and 31.03.2015, NBU announced exchange rate in the middle of the day
    // hence take the previous day's value because currency operations are done before noon    
    // http://www.bank.gov.ua/control/uk/publish/article?art_id=6629632&cat_id=55838
    // https://bank.gov.ua/control/uk/publish/article?art_id=15821785&cat_id=55838
    let exchangeRateDate = if (date >= new DateTime(2014, 4, 4) && date <= new DateTime(2015, 03, 31)) then date.AddDays(-1.0) else date

    let url = String.Format("https://bank.gov.ua/NBUStatService/v1/statdirectory/exchange?valcode={1}&date={0:yyyyMMdd}", exchangeRateDate, currencyCode)
    
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
module IfobsImport

open System
open System.Globalization
open System.IO
open System.Linq
open System.Text.RegularExpressions
open System.Xml.Linq

open Excel

open CalculationCore
open Types

let loadRawReceiptsDataFromExcel detailsColumnNo dateColumnNo amountColumnNo fileName =
    use receiptsFile = File.Open(fileName, FileMode.Open)
    use receiptsReader = ExcelReaderFactory.CreateBinaryReader(receiptsFile)
    
    seq {
        while receiptsReader.Read() do              
            let details = receiptsReader.GetString(detailsColumnNo)
            let dateString = receiptsReader.GetString(dateColumnNo)            
            let amountString = receiptsReader.GetString(amountColumnNo)            

            yield (
                details,                
                dateString,
                amountString                
            ) 
    }

    |> List.ofSeq // materialize sequence before resources are disposed

let loadLocalReceipts fileName =
    let currencySellDetailsRegex = new Regex(@"згідно заяви № (\S+) від (\S+)")

    let parseLocalReceiptRow (details, dateString, amountString: String) =    
        let regexMatch = currencySellDetailsRegex.Match details
        let docNo = regexMatch.Groups.[1].Value
        let docDateString = regexMatch.Groups.[2].Value

        (   
            DateTime.ParseExact(dateString, "dd.MM.yyyy", CultureInfo.InvariantCulture),
            Convert.ToDecimal(amountString),     
            docNo,
            DateTime.ParseExact(docDateString, "dd/MM/yyyy", CultureInfo.InvariantCulture)
        )

    fileName
    |> loadRawReceiptsDataFromExcel 6 7 9
    |> Seq.skip 1       // skip headers    
    |> Seq.map parseLocalReceiptRow   

let loadCurrencyReceipts fileName =
    let parseCurrencyReceiptRow  (details, dateString, amountString: String) =
        (
            DateTime.ParseExact(dateString, "dd.MM.yyyy", CultureInfo.InvariantCulture),
            Convert.ToDecimal(amountString)
        )

    fileName
    |> loadRawReceiptsDataFromExcel 6 7 9
    |> Seq.skip 1       // skip headers    
    |> Seq.map parseCurrencyReceiptRow  

let loadDistributionDocs fileName =
    let xml = File.ReadAllText fileName |> XDocument.Parse
    let rows = xml.Descendants (XName.Get("ROW"))

    let mapRow (node: XElement) =
        let documentNumber = node.Attribute(XName.Get("DOCUMENTNO")).Value
        let documentDateString = node.Attribute(XName.Get("DOCUMENTDATE")).Value
        let documentDate = DateTime.ParseExact(documentDateString, "dd.MM.yyyy", CultureInfo.InvariantCulture)

        (documentDate, documentNumber, 0m, 0m)

    rows |> Seq.map mapRow

let loadSellDocs fileName =
    let xml = File.ReadAllText fileName |> XDocument.Parse
    let rows = xml.Descendants (XName.Get("ROW"))

    let mapRow (node: XElement) =
        let documentNumber = node.Attribute(XName.Get("ORDERNO")).Value
        let documentDateString = node.Attribute(XName.Get("ORDERDATE")).Value
        let amountString = node.Attribute(XName.Get("AMOUNT")).Value

        let documentDate = DateTime.ParseExact(documentDateString, "yyyyMMddTHH:mm:fffff", CultureInfo.InvariantCulture)
        let amount = Convert.ToDecimal(amountString) / 100m

        (documentDate, documentNumber, amount)

    rows |> Seq.map mapRow

let loadDocumentsFromIfobs localCurrencyReceiptsExcelFileName foreignCurrencyReceiptsExcelFileName foreignCurrencyDistributionXmlFileName foreignCurrencySalesFileName =                     
    let localReceipts = loadLocalReceipts localCurrencyReceiptsExcelFileName
    let currencyReceipts =  loadCurrencyReceipts foreignCurrencyReceiptsExcelFileName
    let distributionDocs = loadDistributionDocs foreignCurrencyDistributionXmlFileName
    let sellDocs = loadSellDocs foreignCurrencySalesFileName

    (localReceipts, currencyReceipts, distributionDocs, sellDocs)

let classifyIfobsReceipt distributionDocs sellDocs receipt =
    let (date, amount, documentNo, documentDate) = receipt        
    
    let distributionDoc =
        distributionDocs
        |> Seq.tryFind (fun (docDate, docNumber, _, _) -> docDate = documentDate && docNumber = documentNo)

    let sellDoc =
        sellDocs
        |> Seq.tryFind (fun (docDate, docNumber, _) -> docDate = documentDate && docNumber = documentNo)

    match distributionDoc with
        | Some(_) -> MandatoryCurrencySale(date, amount)
        | None ->
            match sellDoc with
                | Some(_, _, currencyAmount) -> RegularCurrencySale(date, amount, currencyAmount)
                | None -> LocalReceipt(date, amount)    

let sortIfobsReceipt (date, _, _, _) = 
    date
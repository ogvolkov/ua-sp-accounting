module BifitImport

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.RegularExpressions

open Logger
open Types

let logLine index line =
    log "processing line %d" index
    line

let loadDocumentsFromBifit localCurrencyDocumentsCsvFileName foreignCurrencyDocumentsCsvFileName =
    let loadReceiptsFromCsv dateColumnNo debitColumnNo creditColumnNo detailsColumnNo fileName =        
            log "Reading receipts from CSV %s" fileName

            File.ReadLines (fileName, Encoding.GetEncoding(1251))
            |> Seq.skip 1       // skip headers    
            |> Seq.mapi logLine
            |> Seq.map (fun s -> s.Split ';')
            |> Seq.filter (fun a -> a.[creditColumnNo] <> "")
            |> Seq.map (fun a -> (a.[dateColumnNo], a.[creditColumnNo], a.[detailsColumnNo])
            )                 

    let parseReceiptRow (dateString, amountString: String, details) =        
        (   
            DateTime.ParseExact(dateString, "dd.MM.yyyy HH:mm", CultureInfo.InvariantCulture), // optional time dd.MM.yyyy HH:mm
            Convert.ToDecimal (amountString),
            details
        )

    let loadReceipts fileName =
        fileName
        |> loadReceiptsFromCsv 4 13 14 15
        |> Seq.map parseReceiptRow

    let ignoredIncomeDetailsRegex = new Regex("Повернено.*")

    let localReceipts = loadReceipts localCurrencyDocumentsCsvFileName
    let filteredLocalReceipts = localReceipts |> Seq.filter (fun (_, _, details) -> not (ignoredIncomeDetailsRegex.Match details).Success)

    let currencyReceipts =  loadReceipts foreignCurrencyDocumentsCsvFileName
                                |> Seq.map (fun (date, amount, _) -> (date, amount))
    
    (filteredLocalReceipts, currencyReceipts)

let classifyBifitReceipt (date, amount, details) =    
    let voluntarySaleRegex = new Regex(".*продажу валюти ([ .0-9]+) .*(зг.*) заявк?и.*")
    let mandatorySaleRegex = new Regex(".*продажу валюти ([ .0-9]+) .*")
    
    let voluntaryMatch = voluntarySaleRegex.Match details

    if voluntaryMatch.Success then        
        let currentAmountAsString = voluntaryMatch.Groups.[1].Value.Replace(" ", "")
        printfn "try %s\n" currentAmountAsString
        let currencyAmount =  Convert.ToDecimal(currentAmountAsString)

        RegularCurrencySale(date, amount, currencyAmount)
    else
        let mandatoryMatch = mandatorySaleRegex.Match details
        if mandatoryMatch.Success then MandatoryCurrencySale(date, amount)
        else LocalReceipt(date, amount)    

let sortBifitReceipt (date, _, _) =
    date
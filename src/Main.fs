module Main

open CalculationCore
open IfobsImport
open BifitImport
open Types

let ifobsDocuments = loadDocumentsFromIfobs @"..\data\ifobs\uah_statement.xls" @"..\data\ifobs\eur_statement.xls" @"..\data\ifobs\distribution.xml" @"..\data\ifobs\sales.xml"

let bifitDocuments = loadDocumentsFromBifit @"..\data\bifit\uah_statement.csv" @"..\data\bifit\eur_statement.csv"

let (otpLocalReceipts, ifobsCurrencyReceipts, otpDistributionDocs, otpSellDocs) = ifobsDocuments
let (bifitLocalReceipts, bifitCurrencyReceipts) = bifitDocuments

let ifobsValuation = (otpLocalReceipts, ifobsCurrencyReceipts) |> evaluateReceipts (classifyIfobsReceipt otpDistributionDocs otpSellDocs) sortIfobsReceipt
let bifitValuation = (bifitLocalReceipts, bifitCurrencyReceipts) |> evaluateReceipts classifyBifitReceipt sortBifitReceipt

let printIncome overallValuation =
    let sortedValuation = overallValuation |> Seq.sortBy (fun (valuation) ->
            match valuation with
            | LocalValuation (date, _) -> date
            | CurrencyReceiptValuation(date, _, _, _) -> date
            | CurrencySaleValuation(date, _, _, _) -> date
            ) 

    let printValuation valuation =
        match valuation with
        | LocalValuation(date, amount) ->
            printfn "%O     %M Local money received" date amount
        | CurrencyReceiptValuation(date, currencyAmount, exchangeRate, equivalent) ->
            printfn "%O     %M Currency received amount %M exchangeRate %M" date equivalent currencyAmount exchangeRate
        | CurrencySaleValuation(date, currencyAmount, amount, currencyChunks) ->
            let (originalValuation, extra) = calculateCurrencySaleTotals currencyChunks amount
            printfn "%O     %M Currency sold, currency amount %M, local amount %M, originalValuation %M " date extra currencyAmount amount originalValuation

            currencyChunks |> Seq.iter (fun c ->
                printfn "-original date %O, amount %M, original exchange rate %M, original valuation %M" c.originDate c.amount c.originalExchangeRate c.originalValuation)

    sortedValuation |> Seq.iter printValuation

    let income = sortedValuation |> Seq.map (fun valuation ->
            match valuation with
            | LocalValuation(date, amount) ->
                (date, amount)
            | CurrencyReceiptValuation(date, _, _, equivalent) ->
                (date, equivalent)
            | CurrencySaleValuation(date, _, amount, currencyChunks) ->
                let (_, extra) = calculateCurrencySaleTotals currencyChunks amount
                (date, extra)
        )

    let incomeByDay =
        income    
        |> Seq.groupBy (fun (date, amount) -> date.Date)
        |> Seq.map (fun (date, seq) -> (date, (seq |> Seq.fold (fun acc (date, amount) -> acc + amount) 0m)))    

    let printIncome incomes =
        incomes |> Seq.iter ( fun (date, amount) -> printfn "%O %M" date amount)

    printfn "%s" "grouped by day"
    printIncome incomeByDay

    let incomeByMonth =
        income    
        |> Seq.groupBy (fun (date, amount) -> (date.Year, date.Month))
        |> Seq.map (fun (date, seq) -> (date, (seq |> Seq.fold (fun acc (date, amount) -> acc + amount) 0m)))

    printfn "%s" "grouped by month"
    printIncome incomeByMonth

    let incomeByQuarter =
        income    
        |> Seq.groupBy (fun (date, amount) -> (date.Year, 1 + (date.Month - 1) / 3) )
        |> Seq.map (fun (date, seq) -> (date, (seq |> Seq.fold (fun acc (date, amount) -> acc + amount) 0m)))

    printfn "%s" "grouped by quarter"
    printIncome incomeByQuarter

printfn "%s" "aaa"

Seq.append ifobsValuation bifitValuation |> printIncome
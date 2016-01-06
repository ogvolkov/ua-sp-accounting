module CalculationCore

open System
open ExchangeRate
open Types

let calculateRevenueForCurrencyReceipt (date, amount, currency) =
    let nbuExchangeRate = getNBUrate date currency
    let equivalent = amount * nbuExchangeRate
    let UAHAmount = Math.Round (equivalent, 2, MidpointRounding.AwayFromZero)
    (date, UAHAmount)

let evaluateReceipts classifyLocalReceipt sortLocalReceipt (localReceipts, currencyReceipts) =
    let rec receiveCurrency date amount currencyPool = 
        match currencyPool with
        | (headDate, headAmount)::tail ->        
            if date < headDate then (date, amount) :: currencyPool
            else (headDate, headAmount) :: (receiveCurrency date amount tail)
        | [] -> [(date, amount)]


    let rec breakCurrencyDownByOrigin amount currencyPool =
        match currencyPool with
            | (headDate, headAmount) :: tail ->
                if headAmount > amount then ([(headDate, amount)], (headDate, headAmount - amount) :: tail)
                else
                    if headAmount = amount then ([(headDate, amount)], tail)
                    else
                        let (amountList, theRest) = breakCurrencyDownByOrigin (amount - headAmount) tail
                        ((headDate, headAmount) :: amountList, theRest)

            | [] -> failwith "not enough currency to sell"


    let currencyPool =
        currencyReceipts 
        |> Seq.sortBy fst
        |> Seq.fold (fun currencyPool (date, amount) -> receiveCurrency date amount currencyPool) []
  
    let calculateReceiptsValuation (valuation, currencyPool) receipt =
        match receipt with
        | LocalReceipt (date, amount) ->        
            ( LocalValuation(date, amount) :: valuation, currencyPool)
        | MandatoryCurrencySale (date, amount) ->        
            ( LocalValuation(date, amount) :: valuation, currencyPool)
        | CurrencyReceipt (date, amount) ->
            let nbuRate = getNBUrate date "EUR"
            let equivalent = amount * nbuRate
            let equivalentRounded = Math.Round (equivalent, 2, MidpointRounding.AwayFromZero)
            
            ( CurrencyReceiptValuation(date, amount, nbuRate, equivalentRounded) :: valuation, currencyPool)        
        | RegularCurrencySale (date, amount, currencyAmount) ->        
            let (currencyBreakdown, newCurrencyPool) = breakCurrencyDownByOrigin currencyAmount currencyPool

            let currencyChunks =
                currencyBreakdown
                |> Seq.map (fun (originDate, amount) ->
                    let originalExchangeRate = getNBUrate originDate "EUR"   
                    let equivalent = amount * originalExchangeRate
                    let equivalentRounded = Math.Round (equivalent, 2, MidpointRounding.AwayFromZero)
                    { originDate = originDate; amount = amount; originalExchangeRate = originalExchangeRate; originalValuation = equivalentRounded }
                )
                |> List.ofSeq    
            
            ( CurrencySaleValuation(date, currencyAmount, amount, currencyChunks) :: valuation, newCurrencyPool)         

    let classifiedLocalReceipts =
        localReceipts
        |> Seq.sortBy sortLocalReceipt
        |> Seq.map classifyLocalReceipt

    let classifiedCurrencyReceipts = 
        currencyReceipts
        |> Seq.map (fun (date, amount) -> CurrencyReceipt(date, amount))
        

    let (overallValuation, remainingCurrency) =
        Seq.append classifiedCurrencyReceipts classifiedLocalReceipts |> Seq.fold calculateReceiptsValuation ([], currencyPool)

    overallValuation
  

let calculateCurrencySaleTotals currencyChunks amount =
    let originalValuation = currencyChunks |> Seq.fold (fun sum chunk -> sum + chunk.originalValuation) 0m
    let extra = if amount > originalValuation then amount - originalValuation else 0m
    (originalValuation, extra)
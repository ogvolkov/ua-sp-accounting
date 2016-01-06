module Types

open System

type Receipt =
    | LocalReceipt of DateTime * decimal
    | MandatoryCurrencySale of DateTime * decimal
    | RegularCurrencySale of DateTime * decimal * decimal
    | CurrencyReceipt of DateTime * decimal

type CurrencySaleChunk = {
    originDate: DateTime;
    amount: decimal;
    originalExchangeRate: decimal;
    originalValuation: decimal
}

type ReceiptValuation =
    | LocalValuation of date : DateTime * amount: decimal
    | CurrencyReceiptValuation of date: DateTime * currencyAmount: decimal * exchangeRate: decimal * localAmount: decimal
    | CurrencySaleValuation of date: DateTime * currencyAmount: decimal * localAmount: decimal * seq<CurrencySaleChunk>
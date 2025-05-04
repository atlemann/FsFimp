module FsFimp.Units

[<Measure>] type Red
[<Measure>] type Green
[<Measure>] type Blue
[<Measure>] type Temperature
[<Measure>] type Percentage
[<Measure>] type Watt

let intM = LanguagePrimitives.Int32WithMeasure
let floatM = LanguagePrimitives.FloatWithMeasure

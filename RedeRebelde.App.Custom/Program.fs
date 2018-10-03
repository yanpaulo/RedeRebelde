namespace RedeRebelde

// Saiba mais sobre F# em http://fsharp.org
// Veja o projeto 'F# Tutorial' para obter mais ajuda.
module Application =
    [<EntryPoint>]
    let main argv = 
        printfn "%A" (AlgoritmoClassificacao.iris ())
        printfn "%A" (AlgoritmoClassificacao.coluna ())
        printfn "%A" (AlgoritmoClassificacao.dermatologia ())
        printfn "%A" (AlgoritmoClassificacao.cancer ())
        printfn "%A" (AlgoritmoClassificacao.xor ())
        0 // retornar um código de saída inteiro

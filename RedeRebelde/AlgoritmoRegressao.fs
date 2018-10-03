namespace RedeMyLittlePoney

open System
open System.Diagnostics

open Algoritmo
open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open FSharp.Collections.ParallelSeq

module AlgoritmoRegressao =
    
    //Tipos
    //Saída de uma realização
    type Realizacao = { RMSE :float; W: Modelo }
    //Resultado do algoritmo para Regressão
    type Resultado = { RMSE: float; DesvioPadrao: float; Melhor: Realizacao; }
    //Resultado da Busca em Grade
    type ResultadoParametros = { NumeroNeuronios: int; MSE: float }
    
    //Precisão via validação cruzada para quantidade de neurônios X taxa de ajute
    let mseGrid dados numNeuronios   = 
        (dados: Par list) |> ignore
        let secoes = 5
        let tamanhoSecao = dados.Length / secoes

        let precisaoSecao n =
            let head = dados |> List.take (tamanhoSecao * n)
            let secao = dados |> List.skip (tamanhoSecao * n) |> List.take tamanhoSecao
            let tail = dados |> List.skip (tamanhoSecao * (n + 1))

            let treinamento = head @ tail
            let teste = secao

            let m = pesos treinamento numNeuronios 
            
            teste |> List.averageBy (fun t -> t.Y.[0] - (resultado m t.X).[0] |> pow2 )

        [0 .. (secoes - 1)] |> List.map precisaoSecao |> List.average
    
    //Busca em grade de quantidade de neurônios X taxa de ajuste
    let ajusteGrid dados neuronios = 
        let map n =
            let mse = mseGrid dados n 
            let mapping = { NumeroNeuronios = n; MSE = mse }
            mapping
            
        neuronios |> PSeq.map map |> PSeq.minBy (fun r -> r.MSE)
    
    let realizacao dados neuronios =
        sw.Start()
        let parametros = ajusteGrid dados neuronios
        sw.Stop()
        printfn "\nParametros escolhidos: \n%A \n(%A)\n" parametros sw.Elapsed

        let treinamento = 
            let n = dados |> List.length |> float |> (*) 0.8 |> int
            dados |> List.take n

        let teste = dados |> List.except treinamento

        let w = pesos treinamento parametros.NumeroNeuronios
        
        let map par =
            let y = resultado w par.X
            par.Y.[0] - y.[0] |> pow2

        let rmse = teste |> List.map map |> List.average |> Math.Sqrt
        
        { RMSE = rmse; W = w }
    
    //Faz 20 realizações e computa a acurácia, desvio padrão e melhor realização.
    let algoritmo dados neuronios = 
        (dados: Par list) |> ignore
        
        sw.Restart()
        printf "Fazendo realizacoes... "
        
        let map _ = 
            realizacao (dados.SelectPermutation() |> List.ofSeq) neuronios

        let realizacoes =
            [0 .. 5] |> PSeq.map map |> PSeq.toList
    
        let maior = 
            realizacoes |>
            List.minBy (fun r -> r.RMSE)
        
        let media =
            realizacoes |>
            List.averageBy (fun r -> r.RMSE)

        let desvio = 
            realizacoes |>
            List.map (fun r -> r.RMSE) |>
            Statistics.StandardDeviation
        
        sw.Stop()
        printfn "%A\n" sw.Elapsed

        { RMSE = media; DesvioPadrao = desvio; Melhor = maior; }

    let resultado () =
        let inc = 0.1
        let max = 50.0
        let dados = [0.0..inc..max]
        let map x =
            let y = funcaoRegressao x
            //let x = x / max 
            { X = [x] |> vector; Y = [y] |> vector }

        let dados = dados |> List.map map

        let neuronios = [5..10]
        let resultado = algoritmo dados neuronios

        resultado
     
    
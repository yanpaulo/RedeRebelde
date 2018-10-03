namespace RedeRebelde

open System
open System.Diagnostics

open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra

module Algoritmo =
    //Tipos
    
    //Par de Entrada X e saída desejada Y
    type Par = { X: float Vector; Y: float Vector }
    //Modelo contendo as listas de neurônios I (camada oculta) e J (camada de saída)
    type Modelo = { C: float Vector list; W: float Matrix }
    //Parâmetros de entrada para a realização do algoritmo
    type Entrada = { Dados: Par list; Classes: Vector<float> list; NumeroNeuronios: float }
    
    let rng = Random.shared
    let pow x n = Math.Pow(x, n)
    let pow2 x = x * x
    let e = Math.E

    //Funções
    //Função radial com vetores x e centro como parâmetros
    let radial x u =
        (x: float Vector) |> ignore
        let termo = Distance.Euclidean(x, u) |> pow2
        
        pow e -termo
    
    //Saída da camada oculta
    let saidaI c x =
        let s = c |> List.map (fun c -> radial x c)
        1.0 :: s |> vector

    //Resultado para entrada x na rede
    let resultado m x =
        let x = saidaI m.C x
        let res = m.W * x
        res.Map(fun v -> Math.Round(v))
        
    
    //Normalização
    let normaliza x min max =
        (x: float) |> ignore

        (x - min) / (max - min)
    
    //Próximo modelo de pesos a lista "dados" de pares
    let pesos dados numNeuronios  =
        (dados: Par list) |> ignore
        
        //Próximo modelo (função w(n+1))
        let pesos c = 
            let X = dados |> List.map (fun p -> saidaI c p.X) |> Matrix.Build.DenseOfColumnVectors
            let Y = dados |> List.map (fun p -> p.Y) |> Matrix.Build.DenseOfColumnVectors
            
            //let W = Y * ((X.Transpose() * X).Inverse() * X.Transpose())
            let W = Y * X.PseudoInverse()

            { C = c; W = W }
     
        let c = 
            dados.SelectPermutation() |> 
            Seq.take numNeuronios |> 
            Seq.map(fun p -> p.X) |> 
            List.ofSeq

        pesos c


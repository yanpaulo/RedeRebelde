namespace RedeRebelde

open Algoritmo
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra

module DadosXor =
    let classes = [ vector [1.0; 0.0]; vector [0.0; 1.0]]

    let dados = 
        let range min max n =
            let range = max - min
            n * range + min
        
        let mapV (x, y) = 
            vector [x; y]

        let x1 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let y1 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let q1 = Seq.zip x1 y1 |> Seq.map mapV |> List.ofSeq

        let x2 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let y2 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let q2 = Seq.zip x2 y2 |> Seq.map mapV |> List.ofSeq

        let x3 = Random.doubles 50 |> Seq.map (range 0.0 0.5)
        let y3 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let q3 = Seq.zip x3 y3 |> Seq.map mapV |> List.ofSeq
        
        let x4 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let y4 = Random.doubles 50 |> Seq.map (range 0.5 1.0)
        let q4 = Seq.zip x4 y4 |> Seq.map mapV |> List.ofSeq

        let mapClass index c =
            {X = c; Y = classes.[index]}

        let classe1 = q1 @ q4 |> List.map (mapClass 0)
        let classe2 = q2 @ q3 |> List.map (mapClass 1)
        let dados = classe1 @ classe2
        dados
    
    let classesSeq = classes |> seq

    let dadosSeq = dados |> seq
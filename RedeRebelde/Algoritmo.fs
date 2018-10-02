namespace RedeMyLittlePoney

open System
open System.Diagnostics
open System.Collections.Generic

open FSharp.Data
open MathNet.Numerics
open MathNet.Numerics.Random
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open FSharp.Collections.ParallelSeq

module Algoritmo =

    //Tipos
    
    //Par de Entrada X e saída desejada Y
    type Par = { X: float Vector; Y: float Vector }
    //Modelo contendo as listas de neurônios I (camada oculta) e J (camada de saída)
    type Modelo = { C: float Vector list; W: float Matrix }
    //Parâmetros de entrada para a realização do algoritmo
    type Entrada = { Dados: Par list; Classes: Vector<float> list; NumeroNeuronios: float }
    //Saída de uma realização
    type RealizacaoClassificacao = { TaxaAcerto:float; Confusao: float Matrix; W: Modelo }
    //Saída uma realização para Regressão
    type RealizacaoRegressao = { RMSE :float; W: Modelo }
    //Resultado do algoritmo para CLassificação1
    type ResultadoClassificacao = { Acuracia: float; DesvioPadrao: float; Melhor: RealizacaoClassificacao; }
    //Resultado do algoritmo para Regressão
    type ResultadoRegressao = { RMSE: float; DesvioPadrao: float; Melhor: RealizacaoRegressao; }
    //Resultado da Busca em Grade
    type ResultadoParametros = { NumeroNeuronios: int; Precisao: float }
    
    let rng = Random.shared
    let pow x n = Math.Pow(x, n)
    let pow2 x = x * x
    let e = Math.E

    //Funções
    let radial x u =
        (x: float Vector) |> ignore
        (u: float Vector) |> ignore
        let termo = Distance.Euclidean(x, u) |> pow2 |> (*) -0.5
        
        pow e termo
    
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
        (x - min) / (max - min)
    
    //Contador de tempo para medição de performance.
    let sw = new Stopwatch();
    
    //Próximo modelo para o vetor "dados"
    //Implementação back-propagation com tail-recursion
    let pesos dados numSaidas numNeuronios  =
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

    
    //Precisão via validação cruzada para quantidade de neurônios X taxa de ajute
    let precisao dados numSaidas numNeuronios   = 
        (dados: Par list) |> ignore
        let secoes = 5
        let tamanhoSecao = dados.Length / secoes

        let precisaoSecao n =
            let head = dados |> List.take (tamanhoSecao * n)
            let secao = dados |> List.skip (tamanhoSecao * n) |> List.take tamanhoSecao
            let tail = dados |> List.skip (tamanhoSecao * (n + 1))

            let treinamento = head @ tail
            let teste = secao

            let m = pesos treinamento numSaidas numNeuronios 
            let acertos = 
                teste |> 
                List.map (fun t -> resultado m t.X = t.Y) |>
                List.filter (fun r -> r) |>
                List.length |> float
            
            acertos / (float teste.Length)

        [0 .. (secoes - 1)] |> List.map precisaoSecao |> List.average
    
    //Busca em grade de quantidade de neurônios X taxa de ajuste
    let ajusteGrid dados numSaidas neuronios = 
        let map n =
            let precisao = precisao dados numSaidas n 
            let mapping = { NumeroNeuronios = n; Precisao = precisao }
            mapping
            
        neuronios |> PSeq.map map |> PSeq.maxBy (fun r -> r.Precisao)
    
    let realizacao dados classes neuronios =
        let numClasses = classes |> List.length        
        let confusao = DenseMatrix.zero numClasses numClasses
    
        sw.Start()
        let parametros = ajusteGrid dados numClasses neuronios
        sw.Stop()
        printfn "\nParametros escolhidos: \n%A \n(%A)\n" parametros sw.Elapsed

        let treinamento = 
            let n = dados |> List.length |> float |> (*) 0.8 |> int
            dados |> List.take n

        let teste = dados |> List.except treinamento

        let w = pesos treinamento numClasses parametros.NumeroNeuronios
        
        let iter par =
            let y = resultado w par.X
            let index = classes |> List.tryFindIndex (fun e -> e = y)

            match index with
                | Some i -> 
                    let j = classes |> List.findIndex (fun e -> e = par.Y)
                    confusao.[i, j] <- confusao.[i, j] + 1.0
                | None -> ()

        teste |> Seq.iter iter
        
        { TaxaAcerto = confusao.Diagonal().Sum() / float (teste |> Seq.length) ; Confusao = confusao; W = w }
    
    //Faz 20 realizações e computa a acurácia, desvio padrão e melhor realização.
    let algoritmo dados classes neuronios = 
        (dados: Par list) |> ignore
        let numClasses = classes |> List.length
        
        sw.Restart()
        printf "Fazendo realizacoes... "
        
        let map _ = 
            realizacao (dados.SelectPermutation() |> List.ofSeq) classes neuronios

        let realizacoes =
            [0 .. 20] |> PSeq.map map |> PSeq.toList
    
        let maior = 
            realizacoes |>
            List.maxBy (fun r -> r.TaxaAcerto)
        
        let media =
            realizacoes |>
            List.averageBy (fun r -> r.TaxaAcerto)

        let desvio = 
            realizacoes |>
            List.map (fun r -> r.TaxaAcerto) |>
            Statistics.StandardDeviation
        
        sw.Stop()
        printfn "%A\n" sw.Elapsed

        { Acuracia = media; DesvioPadrao = desvio; Melhor = maior; }
     
    //Inicia o algoritmo a partir de um banco de dados CSV fornecido
    let algoritmoCSV db classes colunas neuronios =
        (db : Runtime.CsvFile<CsvRow>) |> ignore
        (classes: Map<string, float Vector>) |> ignore

        let colunaClasse = colunas
        let parse (s: string) = 
            match s with
                | "?" -> 0.0
                | s -> s.Replace(".", ",") |> System.Double.Parse
        
        let rows = db.Rows
        
        let minMax =
            let parseRow (row: CsvRow) = row.Columns|> Seq.take colunas |> Seq.map parse
            let valores = rows |> Seq.collect parseRow
            let min = valores |> Seq.min
            let max = valores |> Seq.max

            (min, max)
        
        let (min, max) = minMax

        let normaliza x = normaliza x min max

        let normaliza s = parse s |> normaliza

        let parseRow (row: CsvRow) = row.Columns |> Seq.take colunas |> Seq.map normaliza |> List.ofSeq

        let mapRow (row: CsvRow) = { X = parseRow row |> vector; Y = classes.[row.[colunaClasse]] |> vector }
    
        let dados = rows |> Seq.map mapRow |> List.ofSeq
        //let classes = classes.Values |> Seq.map (fun e -> vector e) |> List.ofSeq
        let classes = classes |> Map.toList |> List.map (fun (_, v) -> v)


        algoritmo dados classes neuronios

    let algoritmoIris () =
        printfn "Iris"
        let db = CsvFile.Load("iris.data").Cache()
        let classes = Map.ofList [("Iris-setosa", vector [1.0; 0.0; 0.0]); ("Iris-versicolor", vector [0.0; 1.0; 0.0]); ("Iris-virginica", vector [0.0; 0.0; 1.0])]

        let neuronios = [8..10]

        algoritmoCSV db classes 4 neuronios

    let algoritmoColuna () =
        printfn "Coluna Terbreval"
        let db = CsvFile.Load("column_3C.dat", " ").Cache()
        let classes = Map.ofList [("DH", vector [1.0; 0.0; 0.0]); ("SL", vector [0.0; 1.0; 0.0]); "NO", (vector [0.0; 0.0; 1.0])]
        let neuronios = [4 .. 10]

        algoritmoCSV db classes 6 neuronios
    
    let classesMap list = 
        let num = list |> List.length
        let gen index n = 
            let head = List.init (index) (fun _ -> 0.0)
            let tail = List.init (num - head.Length - 1) (fun _ -> 0.0)
            let v = head @ [1.0] @ tail |> vector
            (n.ToString(), v)
        list |> List.mapi gen |> Map.ofList
        
    let algoritmoDermatologia () =
        printfn "Dermatologia"
        let db = CsvFile.Load("dermatology.data").Cache()
        
        let classes = classesMap [1..6]
        let neuronios = [7 .. 10]

        algoritmoCSV db classes 34 neuronios

    let algoritmoCancer () =
        printfn "Câncer de Mama"
        let db = CsvFile.Load("breast-cancer-wisconsin.data").Cache()
        
        let classes = classesMap[2; 4]
        let neuronios = [7 .. 10]

        algoritmoCSV db classes 10 neuronios
    
    let classesXor = [ vector [1.0; 0.0]; vector [0.0; 1.0]]

    let dadosXor = 
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
            {X = c; Y = classesXor.[index]}

        let classe1 = q1 @ q4 |> List.map (mapClass 0)
        let classe2 = q2 @ q3 |> List.map (mapClass 1)
        let dados = classe1 @ classe2
        dados
    
    let classesXorSeq = classesXor |> seq

    let dadosXorSeq = dadosXor |> seq

    let algoritmoXor () =
        printfn "XOR"
        
        let neuronios = [8 .. 10]

        algoritmo dadosXor classesXor neuronios
    
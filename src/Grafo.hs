module Grafo
   (
        sequenciaGraus,
        grauMax,
        coloreVérticesIngênuo,
        principal
   ) where

import GrafoListAdj
import Data.List
import System.Random
import System.Random.Shuffle 
import Control.Monad
import Control.Exception
import Data.Time


--sudo apt-get install cabal-install cabal update
--cabal install random
--sudo apt install libghc-random-shuffle-dev
--wget -qO- https://get.haskellstack.org/ | sh (install haskell)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sequenciaGraus grafo = sort (map (grau grafo) (vértices grafo))

grauMax grafo = last(sequenciaGraus grafo)

criaListaCores grafo = take ((grauMax grafo)+1) [1..]

pegaTodosOsVérticesAdjacentesAoVerticeX grafo verticeX [] = []
pegaTodosOsVérticesAdjacentesAoVerticeX grafo verticeX (x:xs) = resultado
    where
        resultado = if adjacente grafo verticeX x then [x] ++ pegaTodosOsVérticesAdjacentesAoVerticeX grafo verticeX xs else (pegaTodosOsVérticesAdjacentesAoVerticeX grafo verticeX xs)

verificaSeOVerticeXPossuiUmaDadaCor verticeX cor listaDeVérticesECoresNormalizada = resultado
    where
        corDoVérticeX = map snd (filter ((==verticeX).fst) listaDeVérticesECoresNormalizada)
        resultado = if cor `elem` corDoVérticeX then True else False

verificaSeOVerticeXEstáColorido verticeX listaDeVérticesECoresNormalizada = resultado
    where
        verticeXTemCor = map snd (filter ((==verticeX).fst) listaDeVérticesECoresNormalizada)
        resultado = if (length(verticeXTemCor) > 0) then True else False

percorreTodosOsVérticesAdjacentesEVerificaSePossuiADadaCor [] cor listaDeVérticesECoresNormalizada = False
percorreTodosOsVérticesAdjacentesEVerificaSePossuiADadaCor vérticesAdjacentes cor [] = False
percorreTodosOsVérticesAdjacentesEVerificaSePossuiADadaCor (x:xs) cor listaDeVérticesECoresNormalizada = resultado
    where 
        éCorDoVérticeAdjacente = (verificaSeOVerticeXPossuiUmaDadaCor x cor listaDeVérticesECoresNormalizada)
        resultado = if éCorDoVérticeAdjacente then True else if not(null(xs)) then percorreTodosOsVérticesAdjacentesEVerificaSePossuiADadaCor xs cor listaDeVérticesECoresNormalizada else False

--grafo listaVértices cores listaDeVérticesColoridos
percorreVértices grafo [] cores listaDeVérticesColoridos = listaDeVérticesColoridos
percorreVértices grafo (x:xs) cor listaDeVérticesColoridos = resultado
    where
        verticeEstáColorido = verificaSeOVerticeXEstáColorido x listaDeVérticesColoridos
        vérticesAdjacentes = pegaTodosOsVérticesAdjacentesAoVerticeX grafo x (vértices grafo)
        vérticeXPossuiACor = percorreTodosOsVérticesAdjacentesEVerificaSePossuiADadaCor vérticesAdjacentes cor listaDeVérticesColoridos
        verticesColoridos = if (vérticeXPossuiACor || verticeEstáColorido) then listaDeVérticesColoridos else listaDeVérticesColoridos ++ [(x, cor)]
        resultado = percorreVértices grafo xs cor verticesColoridos

percorreCores grafo listaVértices [] listaDeVérticesColoridos = listaDeVérticesColoridos
percorreCores grafo listaVértices (x:xs) listaDeVérticesColoridos = resultado
    where
        percorreVérticesX = percorreVértices grafo listaVértices x listaDeVérticesColoridos
        resultado = percorreCores grafo listaVértices xs percorreVérticesX

coloreVérticesIngênuo grafo listaVértices = resultado
    where
        cores = criaListaCores grafo
        resultado = percorreCores grafo listaVértices cores []

--exampleGraph = novoGrafo 10 [(1,2),(1,3),(2,4),(3,5),(4,5),(1,6),(2,7),(3,8),(4,9),(5,10),(6,9),(6,10),(7,8),(7,10),(8,9)] -- Grafo de Petersen (Primeiro grafo)
--exampleGraph = novoGrafo 7 [(1,2),(1,3),(1,5),(1,7),(2,4),(2,3),(2,5),(2,6),(3,6),(3,7),(3,5),(4,6),(4,5),(4,7),(5,7),(6,7)] -- (Segundo grafo)
--exampleGraph = novoGrafo 6 [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,6),(3,5),(3,6),(4,5),(4,6),(5,6)] -- (Terceiro grafo)
--exampleGraph = novoGrafo 13 [(1,2),(1,5),(1,6),(2,6),(2,7),(2,3),(3,7),(3,4),(3,8),(4,8),(4,9),(4,13),(5,6),(5,10),(6,10),(6,11),(7,11),(7,12),(8,12),(8,13),(9,13),(10,11),(11,12),(12,13)] -- (Quarto grafo)
exampleGraph = novoGrafo 17 [(1,2),(1,5),(2,5),(2,6),(3,6),(3,7),(3,4),(4,7),(5,6),(5,8),(5,9),(6,7),(6,9),(7,9),(7,10),(8,9),(8,11),(9,10),(9,11),(9,12),(9,13),(10,13),(11,14),(11,12),(11,16),(12,13),(12,16),(12,17),(13,15),(13,17),(14,16),(15,17)] -- Quinto grafo

xs = vértices exampleGraph

pegaTamanhoCromático x = length(nub(map snd x))

principal :: IO ()
principal = do
    start <- getCurrentTime

    let verticesGrafo = (vértices exampleGraph)
    resultadoVerticesEmbaralhados <- shuffleM verticesGrafo
        
    let resultadoGrafoColorido = (coloreVérticesIngênuo exampleGraph resultadoVerticesEmbaralhados)

    let numeroCromatico = pegaTamanhoCromático resultadoGrafoColorido

    end <- getCurrentTime

    putStrLn("------------------------")
    putStrLn("Arestas do grafo: ")
    print $ (arestas exampleGraph)
    putStrLn("------------------------")
    putStrLn("Vértices embaralhados: ")
    print $ resultadoVerticesEmbaralhados
    putStrLn("------------------------")
    putStrLn("Grafo colorido [(vértice, cor)]: ")
    print $ resultadoGrafoColorido
    putStrLn("------------------------")
    putStrLn("Número cromático: ")
    print $ numeroCromatico
    putStrLn("------------------------")
    putStrLn("Tempo de execução: ")
    print (diffUTCTime end start)
    putStrLn("------------------------")

\documentclass[a4paper, titlepage, draft]{report}
\usepackage[utf8]{inputenc}
\usepackage{courier} % Required for the courier font
\usepackage[bookmarks]{hyperref}
\usepackage{amsfonts}
\usepackage{verbatim}
%include .cabal-sandbox/share/x86_64-linux-ghc-7.10.3/lhs2tex-1.19/lhs2TeX.fmt

%redefine percentage sign to be a little smaller
\let\oldpct\%
\long\def\ignore#1{}
\renewcommand{\%}{\scalebox{.9}{\oldpct}}
\begin{document}
\setcounter{chapter}{1}

\title{GAER}
\author{
	Sigurt Bladt Dinesen
	\\\texttt{sidi@@itu.dk}
}

\maketitle

\section{Introduction}
This is the lab-report for the first lab for the course \texttt{Artificial Life
\& Evolutionary Robotics: Theory, Methods and Art}. It is written as a literate Haskell program, which means the code will appear intertwined with the text.

\section{Genetic algorithm for solving the Travelling Salesman Problem}
First the Haskell library functions used in this lab:

\begin{code}
import Data.Array (Array, accumArray, indices, (!))
import Data.Maybe (fromJust)
import Data.List (intersect, union, find, sortOn, maximum)
import System.Random (newStdGen, randoms)
\end{code}
\\
There are four main points to define a genetic algorithm. Here are the
definitions used in this lab:

\begin{description}
\item[Genotype and phenotype]
The genotype is just an ordered list of vertexes (city ids).
The interpretation of the genotype (the phenotype) is a path through the graph
representing our instance of TSP, progressing through the vertices in the
genome, returning the first vertex in the end.

the first vertex in the list, and progress by traveling
\begin{code}
type Genome = [Vertex]
\end{code}

\item[Mutation]
The step that takes one generation of genomes, and creates a
new generation of genomes. In this report, mutation is done by sexual
reproduction between two genomes, by random crossover, producing only one child.

I experimented with random mutation as well, but did
not achieve any better results.

\begin{code}
crossOver :: Genome -> Genome -> Int -> Genome
crossOver g h split = take split g ++ drop split h
\end{code}


\begin{code}
mutate :: [(Int, Int)] -> Genome -> [Genome]
mutate (r:rs) genome = replace r genome : mutate rs genome
  where replace (i,j) g = map (\x -> if x==i then j else x) g
\end{code}

\item[Fitness]
Determines the \textit{evolutionary} value of a genome, for use in
\texttt{selection}. The obvious choice for a (fitness) function here is the
travel-cost of the phenotype. Neither mutation nor the definition of the genome
ensures the \textit{validity} of a genome in this solution, i.e. a genome could
look like $\left[1,1,1,1,1,1,1,1,1,1\right]$, which is not just a \textit{bad}
solution, but not a solution at all. So the fitness function must steer the
selection away from such genomes. To achieve this, another term is included in
the fitness function: An exponential function (for smoothness) of the Jaccard
distance between the genome, and the list of all vertexes
($\left[1,2,3,4,5,6,7,8,9,10\right]$). 
The final fitness function of a genome $g$ is defined as

	$$2^{J(g) \cdot T} + pathlength(g)$$

Where $J$ is one minus the Jaccard distance between $g$ and the set of all
vertices, and $T$ is a large constant term to ensure that non-zero Jaccard
distances are weighed heavier than any path length.

\begin{code}
pathCost :: Genome -> Graph Int -> Double
pathCost genome graph = f (genome ++ [head genome]) graph
    where f [x] _ = 0
          f (x:y:xs) graph = getEdgeCost x y graph + f (y:xs) graph

jaccardCost :: Genome -> Graph Int -> Double
jaccardCost genome graph =
  let allVertices = indices graph
      lenDouble = fromIntegral . length
      unionSize :: Double ; unionSize = lenDouble $ union genome allVertices
      intersectSize :: Double ; intersectSize = lenDouble $ intersect genome allVertices
  in 1 - intersectSize/unionSize

cost :: Genome -> Graph Int -> Double
cost genome graph =
  let jaccard = jaccardCost genome graph
      path = pathCost genome graph
  in 2**(jaccard*(10^2+20)) + path
\end{code}

\item[Selection]
The step that takes one generation of genomes, and decides who (or whose children)
gets to be part of the next generation of genomes.
In this project, selection is done by taking the best half of the population, and
letting it, and their children, be the new population.
This does not preserve diversity very well, but seems to produce good results
nonetheless.

\begin{code}
evolve :: [Genome] -> Graph Int -> [Int] -> [IO()]
evolve pop g rInts = evolve_ pop g rInts 1 30
evolve_ _ _ _ _ 0 = [return ()]
evolve_ pop g rInts gen genLeft =
  let (winners, losers) = splitAt 250 $ sortOn (`cost` g) pop
      bestCrossed = map (\(g,h,r) -> crossOver g h r) $ zip3 winners (reverse winners) rInts
      newPop = bestCrossed ++ winners
      output = "Gen " ++ show gen ++ " " ++
               "Best: " ++
               show (cost (head winners) g) ++ " -- " ++
               show (head winners) ++ " " ++
               "Worst: " ++ show (cost (last winners) g) ++ " -- " ++
               show (last winners)
  in putStrLn output : evolve_ newPop g (drop 500 rInts) (gen + 1) (genLeft - 1)
\end{code}
\end{description}

\ignore{
\begin{code}
type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Edge e = (Vertex, e, Vertex)
type Bounds = (Vertex, Vertex)

data LabGraph n e = LabGraph (Graph e) (Vertex -> n)

buildGraph :: Bounds -> [Edge e] -> Graph e
buildGraph bounds edges = accumArray (flip (:)) [] bounds [(v, (l, w)) | (v, l, w) <- edges]

getEdgeCost :: Vertex -> Vertex -> Graph Int -> Double
getEdgeCost v w g = fromIntegral . fst . fromJust $ find ((w==) . snd) vAdjsj
  where vAdjsj = g ! v
\end{code}

\begin{code}
cities = zip [1..10] ['a'..'j']
verts = map fst cities
delta v w = (v-1)*(w-1) + v + w
worldGraph = buildGraph (1,10) [(v, delta v w, w) | v <- verts, w <- verts]
graph = LabGraph worldGraph (fromJust . (flip lookup) cities)


main :: IO ()
main = do
  gen <- newStdGen
  let rInts = map ((1+) . (`mod` 10)) $ randoms gen :: [Int]
      pop = take 500 $ f rInts
        where f l = take 10 l : f (drop 10 l)
  sequence $ evolve pop worldGraph (drop 500 rInts)
  return ()

\end{code}
}
\end{document}


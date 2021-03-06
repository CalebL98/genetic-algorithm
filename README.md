---
title: Genetic Algorithm Example
author: Caleb Lyon
date: 1/16/2020
---

# Genetic Algorithm Example

This folder contains a genetic algorithm implementation that finds a solution to the equation

	y = x1 + x2 + x3 + x4

for a given `y`. Clearly this problem itself is trivial, although the genetic algorithm could be
applied to solve much more challenging problems. In this implementation each `x` value is represented 
in 4 bits (so each `xi` ranges from 0 to 15) and each overall solution is represented as an array of 
16 bits.

The genetic algorithm starts with a random initial population of 20 potential solutions, each of
which is given a fitness score based on how close it is to a real solution. In this case the fitness
score for `(x1,x2,x3,x4)` is given by `|x1 + x2 + x3 + x4 - y|` and a perfect solution has a fitness
score of 0. We keep only the ten solutions with the best fitness scores, and these become the 
"parent" solutions for the next generation.

The next generation is produced by applying mutation and crossover to the parent solutions. Mutation 
iterates over the solution and changes each bit with a given probability p. In the testing of this
solution we use `p = 0.1` so that one tenth of the bits are flipped, but this can be easily changed.

Mutation:

	(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), p = 0.5  =>  (1,0,0,1,0,1,0,1,1,0,1,1,0,1,0,0)

Crossover takes two parents solutions and combines them to create two new solutions by generating
a random index and swapping the parts of the parent solutions after this index. Thus crossover
typically produces more significant changes from the parent solutions than mutation with low 
probability.

Crossover:

	(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)				(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)
					&				, i = 7  =>  					&
	(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)				(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)


Hence mutation and crossover can be applied to create new "child" solutions which are added to 
create the next generation. Taking the top ten solutions from the previous generation as the parent 
solutions and adding the ten children solutions gives us a new generation and the algorithm can 
repeat. In this implementation we only go until one true solution is found, although this code could
be modified to find more possible solutions or until the average fitness score in the final 
generation is below a certain threshold.


This folder contains implementations of this algorithm in both Python3 and Scala, as well as brief
analysis of the time required to run this algorithm for different target values. The Python version
is written in Jupyter notebook and the visual display of results is included within the notebook. 
The Scala version has a Scala file for the actual Scala code and another Microsoft Excel file 
displaying the results of running the code. The Scala file can be executed simply by entering the
command:

	$ scala GeneticAlgorithm.scala



	#   g e n e t i c - a l g o r i t h m  
 
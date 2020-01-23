/*
Author: Caleb Lyon
Purpose: Implement genetic algorithm in Scala to find solution to x1 + x2 + x3 + x4 = y
		 for a given y and analyze runtimes for different y values.
*/

// Takes an array of 4 bits and converts to a decimal integer
def arr_to_dec(arr: Array[Int]): Int = {
	if (arr.length != 4) -1   // If the array is not the appropriate length return -1 to indicate error
	else {
		var sum = 0
		for (i <- 0 until arr.length) {
			// For each bit, add the appropriate power of 2 times the bit
			sum = sum + arr(i) * scala.math.pow(2,3-i).toInt
		}
		sum
	}
}


// Computes the fitness score of a solution (array of 16 bits) for a given y (decimal integer)
def fit(y: Int, sol: Array[Int]): Int = {
	val xs = Array(sol.slice(0,4), sol.slice(4,8), sol.slice(8,12), sol.slice(12,16))
	var sum = 0
	for (x <- xs) sum += arr_to_dec(x)
	scala.math.abs(y - sum) // The difference between the target value and the sum of the four 
							// binary values gives the solution's fitness
}

// Takes an array of 16 bits and a probability of mutating and returns such an array having applied mutation
// Does not modify original solution
def mutate(arr: Array[Int], p: Double): Array[Int] = {
	var new_arr: Array[Int] = Array()	// Creating new array so original isn't modified
	val r = scala.util.Random
	for (bit <- arr) {					// Traverse entire solution and for each:
		if (r.nextDouble() < p) {		// Flip bit with probability p
			if (bit == 0) new_arr = new_arr :+ 1
			else new_arr = new_arr :+ 0
		}
		else new_arr = new_arr :+ bit
	}
	new_arr	
}


// Takes two arrays of 16 bits and returns resulting arrays having applied crossover
// Does not modify original solutions
def crossover(arr1: Array[Int], arr2: Array[Int]) = {
	val r = scala.util.Random
	val i = r.nextInt(16)					// Generates a random index at which to swap solutions
	val new1 = arr1.take(i) ++ arr2.drop(i) // One new solution is first half of first parent and 
											// second half of second parent
	val new2 = arr2.take(i) ++ arr1.drop(i) // Second new solution is first half of second parent and
											// second half of first parent
	(new1, new2)
}




// Inputs:
    // y: target int
    // init_sols: a first generation of solutions
    // mutate_prob: the probability with which we mutate when applying mutation function
// Output:
    // Final generation of solutions

def genetic_algorithm(y: Int, init_sols: Array[Array[Int]], mutate_prob: Double): Array[Array[Int]] = {
	var found_sol = false
	var sols = init_sols
	while (found_sol == false) {
		//sols_with_fit is an array of tuples, where the first elem of the tuple is a solution
		// and the second elem is the solution's fitness score
		var sols_with_fit: Array[(Array[Int], Int)] = Array()
		for (elem <- sols) sols_with_fit = sols_with_fit :+ ((elem, fit(y, elem)))

		//top_ten is an array of the top ten solutions without their fitness scores
		var top_ten: Array[Array[Int]] = Array()
		for (elem <- sols_with_fit.sortBy(_._2).slice(0,10)) {
			top_ten = top_ten :+ elem._1
		}
		
		// Generating new solutions by applying mutation to 6 best of top_ten and crossover to other 4
        // Since mutation typically makes less significant changes this is why mutation is applied to 
		// the better scores
		sols = top_ten
		for (sol <- top_ten.slice(0,6)) sols = sols :+ mutate(sol, mutate_prob)
		val (n1, n2) = crossover(top_ten(6), top_ten(7))
		val (n3, n4) = crossover(top_ten(8), top_ten(9))
		sols = sols ++ Array(n1, n2, n3, n4)
		
		for (i <- 0 until sols.length) {
			if (fit(y, sols(i)) == 0) {
				found_sol = true
				sols = Array(sols(i)) ++ sols.take(i) ++ sols.drop(i+1)
			}
		}
	}
	sols
}



// Generating intial population of 20 sols
def get_random_sols(): Array[Array[Int]] = {
	val r = scala.util.Random
	var init_sols: Array[Array[Int]] = Array()
	for (i <- 0 until 20) {			// Generating 20 solutions
		var sol: Array[Int] = Array()
		for (j <- 0 until 16) {			// Each solution is 16 bits long
			sol = sol :+ r.nextInt(2)
		}
		init_sols = init_sols :+ sol
	}
	init_sols
}

// Testing Genetic Algorithm function
val init_sols = get_random_sols()
println("Running this genetic algorithm on y=5 gives the solutions: ")
for (elem <- genetic_algorithm(5, init_sols, 0.1)) {
	print(elem.slice(0,4).mkString(",") + " ")
	print(elem.slice(4,8).mkString(",") + " ")
	print(elem.slice(8,12).mkString(",") + " ")
	print(elem.slice(12,16).mkString(",") + " ")
	val (x1, x2, x3, x4) = (arr_to_dec(elem.slice(0,4)), arr_to_dec(elem.slice(4,8)),
							arr_to_dec(elem.slice(8,12)), arr_to_dec(elem.slice(12,16)))
	println("(" + x1.toString + "+" + x2.toString + "+" + x3.toString + "+" + x4.toString + 
			"=" + (x1 + x2 + x3 + x4).toString + ")")
}


//Do timing stuff
println()
println("y, Average Runtime")
for (y <- 0 to 60) {
	var sum = 0.0
	for (i <- 0 until 20) {	// For more accurate analysis algorithm is run several 
							// (20 right now) times and resulting time is averaged
		val t1 = System.nanoTime()
		val init_sols = get_random_sols()	// Random initial population 
		genetic_algorithm(y, init_sols, 0.1)	// Run genetic algorithm on these starting solutions
		val time = System.nanoTime() - t1
		sum += time 
	}
	val avg_time = sum / 5
	println(y + ", " + avg_time)
}





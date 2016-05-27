package ch06

/**
  * Created by fernandopratama on 26/5/16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * EX01, write a function to generate positive a random positive integer
    *
    * @param rng The current seed
    * @return Random positive integer, and the next seed
    */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * EX02, write a function to generate a Double between 0 and 1, not
    * including 1
    *
    * @param rng The current seed
    * @return Random double, and the next seed
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /*
    Ex03, write functions to generated (Int, Double) pair, (Double, Int)
    pair, and a (Double, Double, Double) 3-tuple.
     */

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * Write a function to generate a list of random integers
    *
    * @param count The number of random integers
    * @param r     The current seed
    * @return List of random integer and the next seed
    */
  def ints(count: Int)(r: RNG): (List[Int], RNG) = count match {
    case 0 => (List(), r)
    case n =>
      val (x, r1) = r.nextInt
      val (xs, r2) = ints(n - 1)(r1)
      (x :: xs, r2)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * EX06, use map to reimplement RNG.double in a more elegant way
    *
    * @return Random Rand[Double]
    */
  def _double: Rand[Double] =
    map(positiveInt: Rand[Int])(_ / Int.MaxValue.toDouble + 1)

  /**
    * EX07, Write a new combinator that can combine two RNG actions into one
    * using a binary rather than unary function
    *
    * @param ra First Rand
    * @param rb Second Rand
    * @param f  Function that accept two parameters from ra and rb
    * @return Rand from the result of applying element from ra and rb by f
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def randIntDouble: Rand[(Int, Double)] =
    map2(int, _double)((_, _))

  def randDoubleInt: Rand[(Double, Int)] =
    map2(_double, int)((_, _))


  /**
    * Ex08, Implement sequence, for combining a List of transitions into a
    * single transition. Use it to reimplement ints
    *
    * @param fs List of Rand operated on
    * @return Rand containing List
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
    * Ex09, Implement flatMap, then use it to reimplement positive Int
    * @param f Rand operated on
    * @param g Function that takes element in f and apply it to return Rand
    * @return Rand from the result of applying g to element in f
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def _positiveInt: Rand[Int] = {
    flatMap(int) {i =>
    if (i < 0) _positiveInt else unit(i)}
  }

    /*
    Ex10, reimplement map and map2 in terms of flatMap
     */

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  /**
    * Ex05, generate an Int between 0 to n, inclusive
    * @param n maximum value of random Integer
    * @return Random Int ranging between 0 to n inclusive
    */
  def positiveMax(n: Int): Rand[Int] =
  flatMap(positiveInt) {i =>
  val mod = i % n
  if (i + (n - 1) - mod >= 0) unit(mod) else positiveMax(n)}
}




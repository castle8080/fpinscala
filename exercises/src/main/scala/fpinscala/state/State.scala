package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match { case (i, nRng) => (if (i < 0) (i + 1).abs else i, nRng) }

  def double(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match { case (i, nRng) => (i.toDouble / (Int.MaxValue.toDouble + 1), nRng) }

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    rng.nextInt match { case (i, rng) =>
      RNG.double(rng) match { case (d, rng) =>
        ((i, d), rng)  
      }
    }

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    intDouble(rng) match { case ((i, d), rng) => ((d, i), rng) }
    
  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    double(rng) match { case (d1, rng) =>
      double(rng) match { case (d2, rng) =>
        double(rng) match { case (d3, rng) =>
          ((d1, d2, d3), rng)  
        }
      }  
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (Nil, rng)
    else
      ints(count - 1)(rng) match { case (l, rng) =>
        rng.nextInt match { case (i, rng) =>
          (i :: l, rng)  
        }  
      }

  val double2 = map(nonNegativeInt) { i =>
    i.toDouble / (Int.MaxValue.toDouble + 1)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  val doubleDouble2 = map2(double, double)((a,b) => (a,b))
    
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (r, rs) =>
      map2(r, rs) { (x, xs) => x :: xs }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

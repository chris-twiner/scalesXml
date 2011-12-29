package org.scales.utils;

/**
* Collection of functions to aid in creating and using MBeans
*/
object LazyHelpers {

  /** <br/> converts => A to ()=>A, allowing the result to be passed around */
  implicit def delayedXToFnX[A](a: => A): ()=> A = { () =>
     a
  }

  /** <br/> converts => Seq[(A,B)] to ()=>Seq[(A,B)], allowing the result to be passed around */
  implicit def delayedSeqMap[A,B](a: => Seq[(A,B)] ): ()=> Seq[(A,B)] = { () =>
    a
  }
  
}

/**
 * Creates lazy tuples where both _1 and _2 are lazily evaluated
 */
object LazyTuplers {
 
  /**
   * <br/>Lazy evaluation to make a tuple, lets us manage nicely when we choose to.
   * <br/>
   * NOTE this needs to happen all the way down
   */
  class LazyTupler[A](a: => A) {
    // lazy param here too, otherwise it will get created by the --> itself.
    def -->[B](b: => B) : () => (A,B) = { () =>
      (a, b)
    }
  }
    
  /** <br/> converts => A to LazyTupler, enabling the --> syntax */
  implicit def allToLazyTupler[A](a: => A ): LazyTupler[A] = {
    new LazyTupler[A](a)
  }
   
}

/**
 * These lazyhelpers are primarily for function creation
 */
object LazyFnHelpers {
  /**
   * <br/>Lazy evaluation to make a tuple, lets us manage nicely when we choose to.
   * <br/>  Unlike LazyTupler the actual tuple members are functions.
   * NOTE this needs to happen all the way down
   */
  class LazyFnTupler[A](a: () => A) {
    // lazy param here too, otherwise it will get created by the --> itself.
    def -->[B](b: () => B) : () => ( () => A, () => B) = { () =>
      (a, b)
    }
  }
  
  /** <br/> converts => A to LazyFnTupler, enabling the --> syntax */
  implicit def allToLazyFnTupler[A](a: => A ): LazyFnTupler[A] = {
    //import LazyHelpers.delayedXToFnX
    new LazyFnTupler[A]( () => a)
  }
}

/**
 * These lazyhelpers are primarily for function creation, e.g. for callFirst or callAll
 */
object LazyTupleOfFunctionHelpers {
  /**
   * <br/>Lazy evaluation to make a tuple, lets us manage nicely when we choose to.
   * <br/>  Unlike LazyTupler and LazyFnTupler the actual tuple members are functions without the extra function.
   * NOTE this needs to happen all the way down
   */
  class LazyTOFTupler[A](a: () => A) {
    // lazy param here too, otherwise it will get created by the --> itself.
    def -->[B](b: () => B) : ( () => A, () => B) = (a, b)
    
  }
  
  /** <br/> converts => A to LazyTOFTupler, enabling the --> syntax */
  implicit def allToLazyTOFTupler[A](a: () => A ): LazyTOFTupler[A] = {
    new LazyTOFTupler[A](a)
  }

  /** <br/> converts => A to LazyTOFTupler, enabling the --> syntax */
  implicit def lazyToLazyTOFTupler[A](a:  => A ): LazyTOFTupler[A] = {
    new LazyTOFTupler[A](a _)
  }
}
package fr.enst.plnc2013.td1

object TD1 {
  
  def isEven(n: Int): Boolean = (n % 2) == 0;

  def isOdd(n: Int): Boolean = (n % 2) != 0;

  def myWhile[T](cond: => Boolean, f: => T) {
    if (cond) {
      f;
      myWhile(cond, f);
    }
  }

  //def solveQueens(numberOfQueens: Int, f: List[(Int, Int)] => Unit) {
  //  
  //}

}

class ExtSeq[T](s: Seq[T]) {

  def any(f: T => Boolean): Boolean = {
    var b: Boolean = false;
    s.foreach((x: T) => b = f(x) || b);  
    b;
  }  

  def all(f: T => Boolean): Boolean = {
    var b: Boolean = true;
    s.foreach((x: T) => b = f(x) && b);  
    b;
  }  

}

object ExtSeq {

  implicit def toExtSeq[T](s: Seq[T]) = new ExtSeq(s);

}

class ExtCond(cond: => Boolean) {

  def doWhile[T](f: => T) {
    if (cond)
    {
      f;
      doWhile(f);
    }
  }  

}

object ExtCond {

  implicit def toExtCond(cond: => Boolean) = new ExtCond(cond);

}

object Main extends App {

  import TD1._

  // Placer ici le code à exécuter

}

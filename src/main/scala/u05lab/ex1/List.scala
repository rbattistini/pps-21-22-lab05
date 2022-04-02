package u05lab.ex1

import u05lab.ex1.List

import scala.Option
import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] =
    foldRight(list)((h, t) => h :: t)

  def appendR(list: List[A]): List[A] = this match
    case h :: t => h :: t.appendR(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] =
    foldRight(Nil())((h, t) => if predicate(h) then h :: t else t)

  def filterR(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filterR(predicate)
    case _ :: t => t.filterR(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] =
    foldRight(Nil())((h, t) => fun(h) :: t)

  def mapR[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.mapR(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft(Nil())((l, e) => e :: l)

  def zipRight: List[(A, Int)] =
    foldRight(Nil())((h, t) => (h, length - 1 - t.length) :: t)

  def zipRightR: List[(A, Int)] =
    def _zipRight(l: List[A], n: Int): List[(A, Int)] = l match
      case h :: t => (h, n) :: _zipRight(t, n + 1)
      case _ => Nil()

    _zipRight(this, 0)

  def partition(pred: A => Boolean): (List[A], List[A]) =
    (filter(pred), filter(!pred(_)))

  def partitionFold(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil(), Nil()))((h, t) => if pred(h) then (h :: t._1, t._2) else (t._1, h :: t._2))

  def partitionR(pred: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _partitionR(list: List[A], pred: A => Boolean, l1: List[A], l2: List[A]): (List[A], List[A]) = list match
      case h :: t if pred(h) => _partitionR(t, pred, h :: l1, l2)
      case h :: t => _partitionR(t, pred, l1, h :: l2)
      case _ => (l1.reverse(), l2.reverse())

    _partitionR(this, pred, Nil(), Nil())

  def span(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil(), Nil()))(
      (h, t) => if pred(h) then (h :: t._1, t._2) else (Nil(), h :: t._1.append(t._2))
    )

  def spanRec(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t if pred(h) => val r = t.spanRec(pred); (h :: r._1, r._2)
    case _ => (Nil(), this)

  def reduce(f: (A, A) => A): A = this match
    case h :: t => t.foldLeft(h)(f)
    case _ => throw UnsupportedOperationException()

  def reduceR(f: (A, A) => A): A = this match
    case h :: Nil() => h
    case h :: t => f(h, t.reduceR(f))
    case _ => throw UnsupportedOperationException()

  def takeRight(n: Int): List[A] =
    foldRight(Nil())((h, t) => if t.length == n then t else h :: t)

  def takeRightR(n: Int): List[A] = this match
    case h :: t if length == n => h :: t.takeRightR(n - 1)
    case h :: t => t.takeRightR(n)
    case _ => Nil()

  def collect[B](f: PartialFunction[A, B]): List[B] =
    filter(f.isDefinedAt).map(f)

  def collectR[B](f: PartialFunction[A, B]): List[B] = this match
    case h :: t => if f.isDefinedAt(h) then f(h) :: t.collectR(f) else t.collectR(f)
    case _ => Nil()

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

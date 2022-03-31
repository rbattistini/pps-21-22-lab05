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

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
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

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  def indexWhere(pred: A => Boolean): Int =
    var i = 0
    while (i < length) {
      val opt = get(i)
      if(opt.isDefined && pred(opt.get)) return i
      i += 1
    }
    -1

  def indexOf(elem: A): Int = indexWhere(elem == _)

  def findFirstIndex(pred: A => Boolean): Option[Int] =
    @tailrec
    def _find(list: List[A])(pred: A => Boolean): Option[Int] = list match
      case h :: _ if pred(h) => Some(indexOf(h))
      case _ :: t => _find(t)(pred)
      case _ => None

    _find(this)(pred)

  def splitAt(n: Int): (List[A], List[A]) =
    @tailrec
    def _splitAt(list: List[A], n: Int, l1: List[A]): (List[A], List[A]) = list match
      case h :: _ if indexOf(h) == n => (l1, list)
      case h :: t => _splitAt(t, n, h :: l1)
      case _ => (Nil(), Nil())

    _splitAt(this, n, Nil())

  def zipRight: List[(A, Int)] =
    map(e => (e, indexOf(e)))

  def zipRightR: List[(A, Int)] =
    def _zipRight(l: List[A], n: Int): List[(A, Int)] = l match
      case h :: t => (h, n) :: _zipRight(t, n + 1)
      case _ => Nil()

    _zipRight(this, 0)

  def partition(pred: A => Boolean): (List[A], List[A]) =
    (filter(pred), filter(!pred(_)))

  def partitionR(pred: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _partitionR(list: List[A], pred: A => Boolean, l1: List[A], l2: List[A]): (List[A], List[A]) = list match
      case h :: t if pred(h) => _partitionR(t, pred, h :: l1, l2)
      case h :: t => _partitionR(t, pred, l1, h :: l2)
      case _ => (l1.reverse(), l2.reverse())

    _partitionR(this, pred, Nil(), Nil())

  def span(pred: A => Boolean): (List[A], List[A]) =
    def _span(n: Int) = (filter(a => !(!pred(a) && indexOf(a) < n)), filter(a => !pred(a) && indexOf(a) < n))
    findFirstIndex(pred).fold((Nil(), Nil()))(_span)

  def spanR(pred: A => Boolean): (List[A], List[A]) =
    findFirstIndex(pred).fold((Nil(), Nil()))(splitAt)

  def reduce(op: (A, A) => A): A = this match
    case h :: t => t.foldLeft(h)(op)
    case _ => throw UnsupportedOperationException()

  def reduceR(op: (A, A) => A): A = this match
    case h :: Nil() => h
    case h :: t => op(h, t.reduceR(op))
    case _ => throw UnsupportedOperationException()

  def takeRight(n: Int): List[A] =
    filter(length - n <= indexOf(_))

  def takeRightR(n: Int): List[A] =
    @tailrec
    def _takeRight(list: List[A], n: Int): List[A] = list match
      case h :: t if length == n + indexOf(h) + 1 => t
      case _ :: t => _takeRight(t, n)
      case _ => Nil()

    _takeRight(this, n)

  def collect[B](f: PartialFunction[A, B]): List[B] =
    this.filter(f.isDefinedAt).map(f)

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

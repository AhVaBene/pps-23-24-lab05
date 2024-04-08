package polyglot.a01b

import polyglot.{OptionToOptional, Pair}
import util.Optionals.Optional as ScalaOptional
import util.Sequences.Sequence as ScalaSequence
import util.Streams.Stream as ScalaStream
import polyglot.a01b.Logics

import scala.util.Random
import scala.jdk.javaapi.OptionConverters

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var minesSeq = ScalaSequence[Pair[Int, Int]]()
  private var selected = ScalaSequence[Pair[Int, Int]]()
  private val random = Random()

  while (minesSeq.size() < mines) {
    val minePlace = new Pair(random.nextInt(size), random.nextInt(size))
    if !minesSeq.contains(minePlace)
      then minesSeq = minesSeq.concat(ScalaSequence(minePlace))
  }

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val pair = new Pair(x, y)
    if minesSeq.contains(pair)
      then OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
    else {
      selected = selected.concat(ScalaSequence(pair))
      OptionToOptional(ScalaOptional.Just(neighbours(pair)))
    }

  private def neighbours(p: Pair[Int, Int]): Int =
    val x = p.getX
    val y = p.getY
    ScalaStream.iterate(x-1)(_+1).take(3)
      .flatMap(xx =>
        ScalaStream.iterate(y-1)(_+1).take(3).map(yy => new Pair(xx, yy))
      ).filter(p => minesSeq.contains(p)).toList.size()

  def won: Boolean = selected.size()+minesSeq.size() eq size*size

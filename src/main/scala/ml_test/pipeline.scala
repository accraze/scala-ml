package ml_test

trait PipelineOperator [-T, +U] {
  def |> (data: T): Option[U]
}


class _FCT[+T](val _fct: T) {
  def map[U](c: T => U): _FCT[U] = new _FCT[U]( c(_fct))
  def flatMap[U](f: T =>_FCT[U]): _FCT[U] = f(_fct)
  def filter(p: T =>Boolean): _FCT[T] = if( p(_fct) ) new _FCT[T](_fct) else zeroFCT(_fct)
  def reduceLeft[U](f: (U,T) => U)(implicit c: T=> U): U = f(c(_fct),_fct)
  def foldLeft[U](zero: U)(f: (U, T) => U)(implicit c: T=> U): U = f(c(_fct), _fct)
  def foreach(p: T => Unit): Unit = p(_fct)
}


/** Perform an transformation operation
 *
 *  @constructor automatically invoke a given operation.
 *  @param op  PipeOperator instance
 */
class Transform[-T, +U](val op: PipeOperator[T, U]) extends _FCT[Function[T, Option[U]]](op.|>) {
  def |>(data: T): Option[U] = _fct(data)
}


trait PreprocModule[-T, +U] { val preProc: PipeOperator[T, U] }
trait ProcModule[-T, +U] {
  val proc: PipeOperator[T, U]
  class Classification[-T, +U] extends PipeOperator [T,U] { }
}
trait PostprocModule[-T, +U] { val postProc: PipeOperator[T, U] }


/** Workflow Factory
 *
 *  @constructor use self references to stack traits/modules
 */
class WorkFlow[T, U, V, W] {
     self: PreprocModule[T,U] with ProcModule[U,V] with
   PostprocModule[V,W] =>
       def |> (data: T): Option[W] = {
          preProc |> data match {
            case Some(input) => {
             proc |> input match {
               case Some(output) => postProc |> output
               case None => { }
             } }
            case None => { }
          } }
}


/** Samples a function, f, with a frequency 1/samples over the interval [0, 1]
 *
 *  @constructor
 *  @param samples number of sample to take.
 */
class Sampler(val samples: Int) extends PipeOperator[Double => Double, DblVector] {
  override def |> (f: Double => Double): Option[DblVector] =
    Some(Array.tabulate(samples)(n => f(n.toDouble/samples)) )
}


/** Normalizes the data over the range [0, 1] using the Stats class
 *
 *  @constructor normalizes incoming data
 */
class Normalizer extends PipeOperator[DblVector, DblVector] {
  override def |> (data: DblVector): Option[DblVector] =
    Some(Stats[Double](data).normalize)
}


/** extracts the index of the large sample (value 1.0) using the Scala collection method, find
 * @constructor
 */
class Reducer extends PipeOperator[DblVector, Int] {
  override def |> (data: DblVector): Option[Int] = Range(0, data.size) find(data(_) == 1.0)
}


// val dataflow = new Workflow[Double => Double, DblVector, DblVector, Int]
//                 with PreprocModule[Double => Double, DblVector]
//                   with ProcModule[DblVector, DblVector]
//                     with PostprocModule[DblVector, Int] {
//   val preProc: PipeOperator[Double => Double,DblVector] = new Sampler(100)
//   val proc: PipeOperator[DblVector,DblVector]= new Normalizer //1
//   val postProc: PipeOperator[DblVector,Int] = new Reducer//1
// }


class XTSeries[T](label: String, arr: Array[T])

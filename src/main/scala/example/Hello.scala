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

package ml_test

class DT[T,U] extends PipeOperator[XTSeries[T], XTSeries[U]] {
  override def |> : PartialFunction[XTSeries[T], XTSeries[U]]
}

/** Timeseries for a given type
 *
 *  @constructor compute f1 and p/r
 *  @param label name of class
 *  @param arrd data array
 */
class XTSeries[T](label: String, arr: Array[T]) {
  def apply (n:Int): T = arr.apply(n)

  @implicitNotFound("Undefined conversion to DblVector")
  def toDblVector(implicit f: T=>Double):DblVector =arr.map(f(_))

  @implicitNotFound("Undefined conversion to DblMatrix")
  def toDblMatrix(implicit fv: T => DblVector): DblMatrix = arr.map( fv( _ ) )

  def + (n: Int, t: T)(implicit f: (T,T) => T): T = f(arr(n), t)

  def head: T = arr.head

  def drop(n: Int):XTSeries[T] = XTSeries(label,arr.drop(n))

  def map[U: ClassTag](f: T => U): XTSeries[U] = XTSeries[U](label, arr.map( x =>f(x)))

  def foreach( f: T => Unit) = arr.foreach(f)

  def sortWith(lt: (T,T)=>Boolean):XTSeries[T] = XTSeries[T](label, arr.sortWith(lt))

  def max(implicit cmp: Ordering[T]): T = arr.max

  def min(implicit cmp: Ordering[T]): T = arr.min

}


abstract class MovingAverage[T <% Double] extends PipeOperator[XTSeries[T], XTSeries[Double]]

/** Simple Moving Average
 *  set of observations is duplicated
 *  clone is shifted by p observations before being zipped together
 *  init avg array and then flatten all three
 *
 *
 *  @constructor computes a simple moving average
 *  @param period integer number of days
 *  @param num implicit numeric
 */
class SimpleMovingAverage[@specialized(Double) T <% Double](val period: Int)
                         (implicit num: Numeric[T]) extends MovingAverage[T] {
  def |> : PartialFunction[XTSeries[T], XTSeries[Double]]  = {
    case xt: XTSeries[T] if(xt != null && xt.size > 0) => {
      val slider = xt.take(data.size-period).zip(data.drop(period))
      val a0 = xt.take(period).toArray.sum/period
      var a:Double = a0
      val z = Array[Array[Double]](Array.fill(period)(0.0), a,
                                   slider.map(x => {a += (x._2 - x._1)/period
                                                a})
      ).flatten
      XTSeries[Double](z)
    }
  }
}

/** Weighted Moving Average
 *  moving average that computes the weighted average of the last p observations
 *  normalize by sum of weights
 *  period is weights.size
 *
 *  @constructor computes a simple moving average with weights
 *  @param weights
 */
class WeightedMovingAverage[@specialized(Double) T <% Double](val weights: DblVector) extends MovingAverage[T] {
  def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
    case xt: XTSeries[T] if(xt != null && xt.size > 1) => {
      val smoothed = Range(weights.size, xt.size).map(i => {
                                                        xt.toArray.slice(i- weights.size , i)
                                                          .zip(weights)
                                                          .foldLeft(0.0)((s, x) => s + x._1*x._2)
                                                      })
      XTSeries[Double](Array.fill(weights.size)(0.0) ++ smoothed)
    }
  }
}

/** Exponential Moving Average
 *  favors latest values
 *  normalize by sum of weights
 *  period is weights.size
 *
 *  @constructor computes an exp moving average using smoothing value
 *  @param alpha smoothing value
 */
class ExpMovingAverage[@specialized(Double) T <% Double](val alpha: Double) extends MovingAverage[T] {
  def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
    case xt: XTSeries[T] if(xt != null && xt.size > 1) => {
      val alpha_1 = 1-alpha
      var y: Double = data(0)
      xt.map( x => {
               val z = x*alpha + y*alpha_1; y=z; z })
    }
  }

  /** uses nyquist smoothing period
   *
   *  @constructor computes an exp moving average using smoothing value
   *  @param nyquist alpha smoothing value
   */
  def apply[T <% Double](nyquist: Int): ExpMovingAverage[T] = new ExpMovingAverage[T](2/( nyquist + 1))
}


// val p_2 = p >>1
// val w = Array.tabulate(p)(n =>if(n==p_2) 1.0 else 1.0/(Math. abs(n-p_2)+1)) //1
// val weights = w map { _ / w.sum } //2
// val src = DataSource("resources/data/chap3/BAC.csv, false)//3
// val price = src |> YahooFinancials.adjClose //4 val sMvAve = SimpleMovingAverage(p)
// val wMvAve = WeightedMovingAverage(weights)
// val eMvAve = ExpMovingAverage(p)
// val results = price :: sMvAve.|>(price) :: wMvAve.|>(price) :: eMvAve.|>(price) :: List[XTSeries[Double]]() //5
// Val outFile = "output/chap3/mvaverage" + p.toString + ".csv" DataSink[Double]( outFile) |> results //6

package ml_test

/** signature for validation of classification model
 *  computes f1 statistic and precision/recall
 *
 */
trait Validation {
  def f1: Double
  def precisionRecall: (Double, Double)
}


/** Compute F1 statistic and precision recall pairs.
 *
 *  @constructor compute f1 and p/r
 *  @param actualExpected array of actual vs expected
 *  @param tpClass target class for true positive observations
 */
class F1Validation(actualExpected: Array[(Int, Int)], tpClass: Int) extends Validation {
  val counts = actualExpected.foldLeft(new Counter[Label])((cnt, oSeries) => cnt + classify(oSeries._1, oSeries._2))

  lazy val accuracy = {
    val num = counts(TP) + counts(TN)
    num.toDouble/counts.foldLeft(0)( (s,kv)  => s + kv._2)
  }

  lazy val precision = counts(TP).toDouble/(counts(TP) + counts(FP))
  lazy val recall = counts(TP).toDouble/(counts(TP) + counters(FN))

  override def f1: Double = 2.0*precision*recall/(precision + recall)
  override def precisionRecall: (Double, Double) = (precision, recall)


  def classify(actual: Int, expected: Int): Label = {
    if(actual == expected) { if(actual == tpClass) TP else TN }
    else { if (actual == tpClass) FP else FN }
  }

}


object Label extends Enumeration {
  type Label = Value
  val TP, TN, FP, FN = Value
}

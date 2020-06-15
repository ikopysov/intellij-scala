package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.codeInspection.collections.UnzipSingleElementInspection._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText

/**
  * @author t-kameyama
  */
class UnzipSingleElementInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(UnzipSingleElement)
}

private object UnzipSingleElementInspection {
  private val UnzipSingleElement: SimplificationType = new SimplificationType {

    override def hint: String = ScalaInspectionBundle.message("replace.with.map")

    override def getSimplification(e: ScExpression): Option[Simplification] = Some(e).collect {
      case q `.unzip` Seq() `._1` Seq() => (q, 1)
      case q `.unzip` Seq() `._2` Seq() => (q, 2)
      case q `.unzip3` Seq() `._1` Seq() => (q, 1)
      case q `.unzip3` Seq() `._2` Seq() => (q, 2)
      case q `.unzip3` Seq() `._3` Seq() => (q, 3)
    }.map { case (q, index) =>
      val args = createExpressionFromText(s"_._$index")(q.getContext)
      val text = invocationText(q, "map", args)
      replace(e).withText(text).highlightFrom(q)
    }

    private val `._1` = invocation("_1").from(Array("scala.Tuple2", "scala.Tuple3"))
    private val `._2` = invocation("_2").from(Array("scala.Tuple2", "scala.Tuple3"))
    private val `._3` = invocation("_3").from(Array("scala.Tuple3"))
  }

}

package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
  * @author t-kameyama
  */
class FilterSetContainsInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(FilterSetContainsInspection)
}

object FilterSetContainsInspection extends SimplificationType {

  override def hint: String = ScalaInspectionBundle.message("remove.redundant.contains")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case qual `.filter` (set `.contains` Seq()) if isSet(set) =>
        val highlightStart = set.end + 1
        val highlightEnd = expr.end - 1
        Some(replace(expr).withText(invocationText(qual, "filter", set)).highlightRange(highlightStart, highlightEnd))
      case _ => None
    }
  }

}
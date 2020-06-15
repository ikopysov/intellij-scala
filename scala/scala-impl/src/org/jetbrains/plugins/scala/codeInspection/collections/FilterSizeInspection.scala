package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * Nikolay.Tropin
 * 2014-05-05
 */
class FilterSizeInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] =
    Array(FilterSize)
}

object FilterSize extends SimplificationType {

  override def hint: String = ScalaInspectionBundle.message("filter.size.hint")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
   expr match {
      case qual`.filter`(cond)`.sizeOrLength` Seq() =>
        Some(replace(expr).withText(invocationText(qual, "count", cond)).highlightFrom(qual))
      case _ => None
    }
  }
}

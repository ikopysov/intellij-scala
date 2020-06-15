package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * @author Nikolay.Tropin
 */
class MapValuesInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(MapValues)
}

object MapValues extends SimplificationType {
  override def hint: String = ScalaInspectionBundle.message("replace.with.values")

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case qual`.map`(`_._2`())`.toIterator` Seq() if isMap(qual) =>
      val iteratorHint = ScalaInspectionBundle.message("replace.with.valuesIterator")
      Some(replace(expr).withText(invocationText(qual, "valuesIterator")).highlightFrom(qual).withHint(iteratorHint))
    case qual`.map`(`_._2`()) if isMap(qual) =>
      Some(replace(expr).withText(invocationText(qual, "values")).highlightFrom(qual))
    case _ => None
  }
}

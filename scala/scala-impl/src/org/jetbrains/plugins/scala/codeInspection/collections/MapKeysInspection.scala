package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

/**
 * @author Nikolay.Tropin
 */
class MapKeysInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(MapKeys)
}

object MapKeys extends SimplificationType {
  override def hint: String = ScalaInspectionBundle.message("replace.with.keys")

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case qual`.map`(`_._1`())`.toIterator` Seq() if isMap(qual) =>
      val iteratorHint = ScalaInspectionBundle.message("replace.with.keysIterator")
      Some(replace(expr).withText(invocationText(qual, "keysIterator")).highlightFrom(qual).withHint(iteratorHint))
    case qual`.map`(`_._1`())`.toSet` Seq() if isMap(qual) =>
      val setHint = ScalaInspectionBundle.message("replace.with.keySet")
      Some(replace(expr).withText(invocationText(qual, "keySet")).highlightFrom(qual).withHint(setHint))
    case qual`.map`(`_._1`()) if isMap(qual) =>
      Some(replace(expr).withText(invocationText(qual, "keys")).highlightFrom(qual))
    case _ => None
  }
}

package org.jetbrains.plugins.scala
package codeInspection
package collections

import com.intellij.codeInspection.ProblemHighlightType
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScTypeExt
import org.jetbrains.plugins.scala.lang.psi.types.result._

/**
 * @author Nikolay.Tropin
 */

object RedundantCollectionConversion extends SimplificationType {
  override def hint: String = ScalaInspectionBundle.message("redundant.collection.conversion")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    val typeAfterConversion = expr.`type`().getOrAny

    // note:
    // will match <Seq(1, 2).to> and <Seq(1, 2).to[List]> but not <Seq(1, 2).to>[List]
    // because of a check in MethodRepr in `.toCollection`
    expr match {
      case (base@Typeable(baseType)) `.toCollection` Seq() if baseType.conforms(typeAfterConversion) =>
        val simplification = replace(expr).withText(base.getText).highlightFrom(base)
        Some(simplification)
      case _ => None
    }
  }
}

class RedundantCollectionConversionInspection extends OperationOnCollectionInspection {
  override def highlightType = ProblemHighlightType.LIKE_UNUSED_SYMBOL

  override def possibleSimplificationTypes: Array[SimplificationType] = Array(RedundantCollectionConversion)
}

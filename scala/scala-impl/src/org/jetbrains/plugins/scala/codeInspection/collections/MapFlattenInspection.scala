package org.jetbrains.plugins.scala
package codeInspection
package collections

import org.jetbrains.plugins.scala.extensions.BooleanExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

/**
 * @author Nikolay.Tropin
 */
class MapFlattenInspection extends OperationOnCollectionInspection {
  override def possibleSimplificationTypes: Array[SimplificationType] = Array(MapFlatten)
}

object MapFlatten extends SimplificationType {
  override def hint: String = ScalaInspectionBundle.message("replace.map.flatten.with.flatMap")

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    expr match {
      case qual`.map`(f)`.flatten` Seq() =>
        val newText = invocationText(qual, "flatMap", f)
        sameType(expr, newText).option {
          replace(expr).withText(newText).highlightFrom(qual)
        }
      case _ => None
    }
  }

  private def sameType(expr: ScExpression, text: String): Boolean = {
    val newExpr = ScalaPsiElementFactory.createExpressionWithContextFromText(text, expr.getContext, expr)
    expr.`type`().exists { oldType =>
      newExpr.`type`().exists { newType =>
        oldType.equiv(newType)
      }
    }
  }
}

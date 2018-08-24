package org.jetbrains.plugins.scala
package annotator
package template

import com.intellij.lang.annotation.AnnotationHolder
import org.jetbrains.plugins.scala.annotator.quickfix.ImplementMethodsQuickFix
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition}
import org.jetbrains.plugins.scala.overrideImplement.{ScalaOIUtil, ScalaTypedMember}

/**
  * Pavel Fatin
  */
object ObjectCreationImpossible extends AnnotatorPart[ScTemplateDefinition] {
  def annotate(definition: ScTemplateDefinition, holder: AnnotationHolder, typeAware: Boolean) {
    if (!typeAware) return

    val isNew = definition.isInstanceOf[ScNewTemplateDefinition]
    val isObject = definition.isInstanceOf[ScObject]

    if (!isNew && !isObject) return

    val refs = superRefs(definition)

    val hasAbstract = refs.exists {
      case (_, clazz) => isAbstract(clazz)
    }

    if (hasAbstract) {
      refs.headOption.map(_._1).foreach { refElement =>
        import ScalaOIUtil._

        val undefined = for {
          member <- getMembersToImplement(definition)
          if member.isInstanceOf[ScalaTypedMember] // See SCL-2887
        } yield {
          try {
            (member.getText, member.getParentNodeDelegate.getText)
          } catch {
            case iae: IllegalArgumentException =>
              throw new RuntimeException("member: " + member.getText, iae)
          }
        }

        if (undefined.nonEmpty) {
          val element = if (isNew) refElement else definition.asInstanceOf[ScObject].nameId
          val annotation = holder.createErrorAnnotation(element, message(undefined.toSeq: _*))
          annotation.registerFix(new ImplementMethodsQuickFix(definition))
        }
      }
    }
  }

  def message(members: (String, String)*): String =
    s"Object creation impossible, since " + members.map {
      case (first, second) => s" member $first in $second is not defined"
    }.mkString("; ")
}

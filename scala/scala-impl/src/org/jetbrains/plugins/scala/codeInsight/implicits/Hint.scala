package org.jetbrains.plugins.scala.codeInsight.implicits

import com.intellij.openapi.editor.{Inlay, InlayModel}
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElement

case class Hint(parts: Seq[Text], element: PsiElement, suffix: Boolean,
                leftGap: Boolean = true, rightGap: Boolean = true,
                menu: Option[String] = None) {

  def addTo(model: InlayModel): Inlay = {
    val inlay = {
      val offset = if (suffix) element.getTextRange.getEndOffset else element.getTextRange.getStartOffset
      val renderer = new TextRenderer(parts, leftGap, rightGap, menu)
      model.addInlineElement(offset, suffix, renderer)
    }
    inlay.putUserData(Hint.ElementKey, element)
    inlay
  }
}

object Hint {
  private val ElementKey: Key[PsiElement] = Key.create("SCALA_IMPLICIT_HINT_ELEMENT")

  def elementOf(inlay: Inlay): PsiElement = ElementKey.get(inlay)
}
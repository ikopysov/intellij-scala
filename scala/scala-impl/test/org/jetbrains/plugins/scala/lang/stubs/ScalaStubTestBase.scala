package org.jetbrains.plugins.scala.lang.stubs

import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.stubs.StubElement
import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.extensions.TraversableExt

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

abstract class ScalaStubTestBase extends SimpleTestCase {

  def doTest[Stub <: StubElement[_] : ClassTag](fileText: String)(stubCheck: Stub => Unit): Unit = {
    val psiFile = parseText(fileText)
    val stubTree = psiFile.asInstanceOf[PsiFileImpl].calcStubTree()
    val list = stubTree.getPlainList.asScala
    list.filterBy[Stub].foreach(stubCheck)
  }
}

package org.jetbrains.plugins.scala
package lang
package psi
package stubs
package elements

import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{IndexSink, StubElement, StubInputStream, StubOutputStream}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDeclaration, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScTypeAliasStubImpl
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

/**
  * User: Alexander Podkhalyuzin
  * Date: 18.10.2008
  */
abstract class ScTypeAliasElementType[Func <: ScTypeAlias](debugName: String)
  extends ScStubElementType[ScTypeAliasStub, ScTypeAlias](debugName) {
  override def serialize(stub: ScTypeAliasStub, dataStream: StubOutputStream): Unit = {
    dataStream.writeName(stub.getName)
    dataStream.writeOptionName(stub.typeText)
    dataStream.writeOptionName(stub.lowerBoundText)
    dataStream.writeOptionName(stub.upperBoundText)
    dataStream.writeBoolean(stub.isLocal)
    dataStream.writeBoolean(stub.isDeclaration)
    dataStream.writeBoolean(stub.isStableQualifier)
    dataStream.writeOptionName(stub.stableQualifier)
  }

  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScTypeAliasStub = {
    new ScTypeAliasStubImpl(parentStub.asInstanceOf[StubElement[PsiElement]], this,
      name = dataStream.readNameString,
      typeText = dataStream.readOptionName,
      lowerBoundText = dataStream.readOptionName,
      upperBoundText = dataStream.readOptionName,
      isLocal = dataStream.readBoolean,
      isDeclaration = dataStream.readBoolean,
      isStableQualifier = dataStream.readBoolean,
      stableQualifier = dataStream.readOptionName)
  }

  override def createStubImpl(alias: ScTypeAlias, parentStub: StubElement[_ <: PsiElement]): ScTypeAliasStub = {
    val maybeAlias = Option(alias)

    val aliasedTypeText = maybeAlias.collect {
      case definition: ScTypeAliasDefinition => definition
    }.flatMap {
      _.aliasedTypeElement
    }.map {
      _.getText
    }

    val maybeDeclaration = maybeAlias.collect {
      case declaration: ScTypeAliasDeclaration => declaration
    }
    val lowerBoundText = maybeDeclaration.flatMap {
      _.lowerTypeElement
    }.map {
      _.getText
    }
    val upperBoundText = maybeDeclaration.flatMap {
      _.upperTypeElement
    }.map {
      _.getText
    }

    val maybeContainingClass = maybeAlias.map(_.containingClass)

    val stableQualifier = maybeContainingClass.collect {
      case obj: ScObject if ScalaPsiUtil.hasStablePath(alias) =>
        obj.qualifiedName + "." + alias.name
    }

    new ScTypeAliasStubImpl(
      parentStub,
      this,
      name              = alias.name,
      typeText          = aliasedTypeText,
      lowerBoundText    = lowerBoundText,
      upperBoundText    = upperBoundText,
      isLocal           = maybeContainingClass.isEmpty,
      isDeclaration     = maybeDeclaration.isDefined,
      isStableQualifier = stableQualifier.isDefined,
      stableQualifier   = stableQualifier
    )
  }

  override def indexStub(stub: ScTypeAliasStub, sink: IndexSink): Unit = {
    val name = stub.getName
    sink.occurrence(index.ScalaIndexKeys.TYPE_ALIAS_NAME_KEY, name)
    if (stub.isStableQualifier) {
      sink.occurrence(index.ScalaIndexKeys.STABLE_ALIAS_NAME_KEY, name)
      stub.stableQualifier.foreach(
        fqn => sink.occurrence[ScTypeAlias, Integer](index.ScalaIndexKeys.STABLE_ALIAS_FQN_KEY, ScalaNamesUtil.cleanFqn(fqn).hashCode)
      )
    }
  }
}
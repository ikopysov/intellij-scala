package org.jetbrains.plugins.scala
package testingSupport.test.scalatest

import com.intellij.execution.configurations._
import com.intellij.openapi.project.Project
import com.intellij.psi.search.ProjectScope
import com.intellij.psi.{PsiClass, PsiModifier}
import com.intellij.testIntegration.TestFramework
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, PsiMethodExt, PsiTypeExt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScModifierListOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScTypeExt, ScalaType}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration.{SettingMap, TestFrameworkRunnerInfo}
import org.jetbrains.plugins.scala.testingSupport.test._
import org.jetbrains.plugins.scala.testingSupport.test.sbt.{SbtCommandsBuilder, SbtCommandsBuilderBase, SbtTestRunningSupport, SbtTestRunningSupportBase}
import org.jetbrains.plugins.scala.testingSupport.test.scalatest.ScalaTestRunConfiguration.DoNotDiscoverAnnotationFqn
import org.jetbrains.sbt.shell.SbtShellCommunication

import scala.concurrent.Future

class ScalaTestRunConfiguration(
  project: Project,
  configurationFactory: ConfigurationFactory,
  name: String
) extends AbstractTestRunConfiguration(
  project,
  configurationFactory,
  name
) {

  override val suitePaths: List[String] = ScalaTestUtil.suitePaths

  override val testFramework: TestFramework = TestFramework.EXTENSION_NAME.findExtension(classOf[ScalaTestTestFramework])

  override val configurationProducer: ScalaTestConfigurationProducer = TestConfigurationUtil.scalaTestConfigurationProducer

  override protected def validityChecker: SuiteValidityChecker = ScalaTestRunConfiguration.validityChecker

  override protected[test] def canBeDiscovered(clazz: PsiClass): Boolean = !clazz.hasAnnotation(DoNotDiscoverAnnotationFqn)

  override protected val runnerInfo: TestFrameworkRunnerInfo = TestFrameworkRunnerInfo(
    classOf[org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner].getName
  )

  override val sbtSupport: SbtTestRunningSupport = new SbtTestRunningSupportBase {

    override def allowsSbtUiRun: Boolean = true

    override def commandsBuilder: SbtCommandsBuilder = new SbtCommandsBuilderBase {
      override def testNameKey: Option[String] = Some("-- -t")
    }

    override def modifySbtSettingsForUi(comm: SbtShellCommunication): Future[SettingMap] = {
      val module = getModule
      for {
        x <- modifySbtSetting(comm, module, SettingMap(), "testOptions", "test", "Test", """Tests.Argument(TestFrameworks.ScalaTest, "-oDU")""", !_.contains("-oDU"))
        y <- modifySbtSetting(comm, module, x, "parallelExecution", "test", "Test", "false", !_.contains("false"), shouldSet = true)
      } yield y
    }
  }
}

object ScalaTestRunConfiguration {

  private def WrapWithAnnotationFqn = "org.scalatest.WrapWith"
  private val DoNotDiscoverAnnotationFqn = "org.scalatest.DoNotDiscover"

  private val validityChecker = new SuiteValidityCheckerBase {
    override def hasSuitableConstructor(clazz: PsiClass): Boolean = {
      val hasConfigMapAnnotation = clazz match {
        case classDef: ScTypeDefinition => classDef.hasAnnotation(WrapWithAnnotationFqn)
        case _                          => false
      }
      if (hasConfigMapAnnotation) {
        !lackConfigMapConstructor(clazz)
      } else {
        super.hasSuitableConstructor(clazz)
      }
    }
  }

  private def lackConfigMapConstructor(clazz: PsiClass): Boolean = {
    implicit val project: ProjectContext = clazz.projectContext

    val constructors = clazz match {
      case c: ScClass => c.secondaryConstructors.filter(_.isConstructor).toList ::: c.constructor.toList
      case _ => clazz.getConstructors.toList
    }

    for (con <- constructors) {
      if (con.isConstructor && con.getParameterList.getParametersCount == 1) {
        con match {
          case owner: ScModifierListOwner =>
            if (owner.hasModifierProperty(PsiModifier.PUBLIC)) {
              val params = con.parameters
              val firstParam = params.head
              val psiManager = ScalaPsiManager.instance
              val mapPsiClass = psiManager.getCachedClass(ProjectScope.getAllScope(project), "scala.collection.immutable.Map").orNull
              val mapClass = ScalaType.designator(mapPsiClass)
              val paramClass = firstParam.getType.toScType()
              val conformanceType = paramClass match {
                case parameterizedType: ScParameterizedType => parameterizedType.designator
                case _ => paramClass
              }
              if (conformanceType.conforms(mapClass))
                return false
            }
          case _ =>
        }
      }
    }

    true
  }
}
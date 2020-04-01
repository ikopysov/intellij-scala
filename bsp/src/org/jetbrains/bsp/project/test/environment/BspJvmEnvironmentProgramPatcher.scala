package org.jetbrains.bsp.project.test.environment

import com.intellij.execution.Executor
import com.intellij.execution.configurations.{JavaParameters, ModuleBasedConfiguration, RunConfigurationModule, RunProfile}
import com.intellij.execution.runners.JavaProgramPatcher
import com.intellij.openapi.util.UserDataHolderBase
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration

import scala.collection.JavaConverters._

class BspJvmEnvironmentProgramPatcher extends JavaProgramPatcher {

  override def patchJavaParameters(executor: Executor, configuration: RunProfile, javaParameters: JavaParameters): Unit = {
    configuration match {
      case testConfig: UserDataHolderBase =>
        val env = testConfig.getUserData(BspFetchTestEnvironmentTask.jvmTestEnvironmentKey)
        if (env != null) {
          val oldEnvironmentVariables = javaParameters.getEnv.asScala.toMap
          val newEnvironmentVariables = oldEnvironmentVariables ++ env.environmentVariables
          javaParameters.setEnv(newEnvironmentVariables.asJava)

          val oldClasspath = javaParameters.getClassPath.getPathList.asScala.toList
          val newClassPath = env.classpath ++ oldClasspath
          javaParameters.getClassPath.clear()
          javaParameters.getClassPath.addAll(newClassPath.asJava)

          javaParameters.setWorkingDirectory(env.workdir)
          javaParameters.getVMParametersList.addAll(env.jvmOptions.asJava)
        }
      case _ =>
    }
  }
}
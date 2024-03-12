import com.typesafe.sbt.GitPlugin.autoImport.git
import com.typesafe.sbt.SbtGit.GitKeys
import sbt.{ Def, Keys, SettingKey }

object Versioning {
  lazy val isRelease = SettingKey[Boolean]("isRelease", "Whether current version is release")

  lazy val settings = Seq(
    isRelease := scala.util.Try(sys.env("ARKON_RELEASE").toBoolean).getOrElse(false),
    Keys.version := DynSettings.chooseVersion.value
  )

  object DynSettings {

    lazy val chooseVersion = Def.settingDyn {
      val version = if (isRelease.value) releaseVersion else devVersion
      Def.setting(version.value)
    }

    lazy val devVersion = Def.setting {
      val base = git.baseVersion.?.value.map(_ + "-").getOrElse("")
      val sha8 = GitKeys.gitReader.value.withGit(
        _.asInstanceOf[com.typesafe.sbt.git.JGit].headCommit.map(_.abbreviate(8).name).getOrElse("")
      )
      val suffix = if (git.gitUncommittedChanges.value) "-SNAPSHOT" else ""
      s"$base$sha8$suffix"
    }

    lazy val releaseVersion = Def.setting {
      require(!git.gitUncommittedChanges.value, "Release version must not have uncommited changes!")
      git.baseVersion.?.value getOrElse sys.error("No base version set")
    }
  }
}
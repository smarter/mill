package mill.bspserver

import java.io._
import java.net.Socket

import scala.collection.JavaConverters._

import ammonite.ops._
import ammonite.runtime.SpecialClassLoader
import coursier.{Cache, CoursierPaths, Repository}

import mill.define._
import mill.eval.{Evaluator, PathRef, Result}
import mill.util.Ctx.{Home, Log}
import mill.util.Strict.Agg
import mill.util.{Loose, Strict}
import mill.util.ParseArgs
import mill.{T, scalalib}
import scalalib.{CompilationResult, Lib, JavaModule, ScalaModule}


import io.circe.Json        // to construct Json values
import scala.meta.jsonrpc._ // for JSON-RPC APIs
// import scala.meta.lsp._     // for LSP endpoints
import scribe._             // for logging

import ch.epfl.scala.bsp.endpoints.{BuildTarget => BuildTargetEndpoint, _}
import ch.epfl.scala.bsp._
import ScalaBuildTarget.encodeScalaBuildTarget

import monix.eval.Task

object BspServer extends ExternalModule {
  def bspServer(ev: Evaluator[Any]) = T.command {
    new Server(ev)
    ()
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  lazy val millDiscover = Discover[this.type]
}

object Server {
  final val MillProtocol = "mill://"
}

// class Server(ctx: Log with Home, evaluator: Evaluator[_], rootModule: BaseModule) {
class Server(evaluator: Evaluator[_]) {
  import Server._

  val rootModule = evaluator.rootModule

  // val evaluator = new Evaluator(ctx.home, pwd / 'out, pwd / 'out, rootModule, ctx.log)

  def buildTargetIdentifier(mod: Module): BuildTargetIdentifier = {
    val path = mod.millModuleSegments.render
    BuildTargetIdentifier(Uri(MillProtocol + path))
  }

  def buildTarget(mod: JavaModule): BuildTarget = {
    BuildTarget(
      id = buildTargetIdentifier(mod),
      displayName = mod.toString,
      kind = BuildTargetKind.Library,
      languageIds = List("scala"),
      dependencies = mod.moduleDeps.map(buildTargetIdentifier).toList,
      capabilities = BuildTargetCapabilities(canCompile = true, canTest = false, canRun = false),
      data =
        mod match {
          case mod: ScalaModule =>
            Some(encodeScalaBuildTarget(scalaBuildTarget(mod)))
          case _ =>
            None
        }
    )
}

  def scalaBuildTarget(mod: ScalaModule): ScalaBuildTarget = {
    val (scalaOrganization: String) +: (scalaVersion: String) +: Nil =
      evaluator.evaluate(Agg(mod.scalaOrganization, mod.scalaVersion)).values
    val scalaBinaryVersion = Lib.scalaBinaryVersion(scalaVersion)

    ScalaBuildTarget(
      scalaOrganization = scalaOrganization,
      scalaVersion = scalaVersion,
      scalaBinaryVersion = scalaBinaryVersion,
      platform = ScalaPlatform.Jvm,
      jars = Nil
    )
  }

  def buildTargets(): List[BuildTarget] = {
    val modules = rootModule.millInternal.segmentsToModules.values
      .collect { case mod: JavaModule => mod }

    modules.map(buildTarget).toList
  }

  def parseId(id: BuildTargetIdentifier): JavaModule = {
    val uri = id.uri.value
    assert(uri.startsWith(MillProtocol))
    ParseArgs(Seq(uri.stripPrefix(MillProtocol)), multiSelect = false) match {
      case Right((List((None, segments)), Nil)) =>
        rootModule.millInternal.segmentsToModules(segments).asInstanceOf[JavaModule]
    }
  }

  def compile(mod: JavaModule) = {
    println("#mod: " + mod)
    // FIXME: if something is Skipped, evaluate just return Skipped (reproduce by removing testArgs in build.sc)
    val x = evaluator.evaluate(Agg(mod.compile)).values.head.asInstanceOf[CompilationResult]
    println("#x: " + x)
    x
  }

  def myServices(logger: LoggerSupport, client: LanguageClient): Services = {
    Services
      .empty(logger)
      .request(Build.initialize) { params =>
        logger.info(params.toString)
        InitializeBuildResult(BuildServerCapabilities(
          compileProvider = CompileProvider(List("scala")),
          testProvider = TestProvider(Nil),
          runProvider = RunProvider(Nil),
          textDocumentBuildTargetsProvider = false,
          dependencySourcesProvider = false,
          resourcesProvider = false,
          buildTargetChangedProvider = false
        ))
      }
      .notification(Build.initialized) { _ =>
        logger.info("Client is initialized")
      }
      .request(Build.shutdown) { _ =>
        logger.info("Client is about to call exit soon")
      }
      .notification(Build.exit) { _ =>
        logger.info("Goodbye!")
        System.exit(0)
      }
      .request(Workspace.buildTargets) { _ =>
        WorkspaceBuildTargets(buildTargets())
      }
      .request(BuildTargetEndpoint.compile) { case CompileParams(ids, originId, arguments) =>
        val modules = ids.map(parseId)
        println("ids: " + ids)
        println("mod: " + modules)
        modules.map(compile)
        CompileResult(originId, None)
      }
  }

  def run() = {
    // TODO: pool size
    implicit val scheduler =
      monix.execution.Scheduler(java.util.concurrent.Executors.newFixedThreadPool(2))

    val io = new InputOutput(System.in, System.out)

    // val clientLogger = Logger("client-logger").withHandler(writer = scribe.writer.FileWriter.simple())

    val serverLogger = Logger("bsp-server").withHandler(writer = scribe.writer.FileWriter.simple())
    val clientLogger = Logger("bsp-client").withHandler(writer = scribe.writer.FileWriter.simple())

    val source = new PipedOutputStream
    val sink = new PipedInputStream
    sink.connect(source)

    implicit val client =
      LanguageClient.fromOutputStream(/*io.out*/ source, clientLogger)

    val messages =
      BaseProtocolMessage.fromInputStream(/*io.in*/ sink, serverLogger)
    val server =
      new LanguageServer(messages, client, myServices(serverLogger, client), scheduler, serverLogger)

    val connection = Connection(client, server.startTask.executeWithFork.runAsync)

    val res = (for {
      // init <- Build.initialize.request(InitializeBuildParams(Uri("file:///bla"), BuildClientCapabilities(languageIds = List("scala"), providesFileWatching = false)))
      // _ <- Task.now(println("init: " + init))
      // _ <- Task.fromFuture(Build.initialized.notify(InitializedBuildParams()))

      targets <- Workspace.buildTargets.request(WorkspaceBuildTargetsRequest())
      _ <- Task.now(println("targets: " + targets))
      _ <- BuildTargetEndpoint.compile.request(CompileParams(targets.right.get.targets.map(_.id), Some("0"), Nil))

      // _ <- Build.shutdown.request(Shutdown())
    } yield ()).runAsync

    scala.concurrent.Await.result(
      res,
      scala.concurrent.duration.Duration.Inf
    )
  }
}

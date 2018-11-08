package mill.bspserver

import java.io._
import java.net.Socket
import java.util.concurrent.CompletableFuture

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
import mill.util.MultiLogger
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
  def bspTcpServer(ev: Evaluator, host: String, port: Int) = T.command {
    val socket = new Socket(host, port)
    val io = new InputOutput(socket.getInputStream, socket.getOutputStream)
    val server = new Server(ev, io)
    server.waitUntilExit()
  }

  implicit def millScoptEvaluatorReads[T] = new mill.main.EvaluatorScopt[T]()
  lazy val millDiscover = Discover[this.type]
}

object Server {
  final val MillProtocol = "mill:"
}

// class Server(ctx: Log with Home, evaluator: Evaluator[_], rootModule: BaseModule) {
class Server(origEvaluator: Evaluator, io: InputOutput) {
  import Server._

  // TODO: pool size
  implicit val scheduler: monix.execution.Scheduler =
    monix.execution.Scheduler(java.util.concurrent.Executors.newFixedThreadPool(2))

  val serverLogger = Logger("bsp-server").withHandler(writer = scribe.writer.FileWriter.simple())
  val clientLogger = Logger("bsp-client").withHandler(writer = scribe.writer.FileWriter.simple())

  implicit val client: LanguageClient =
    LanguageClient.fromOutputStream(io.out, clientLogger)

  val messages =
    BaseProtocolMessage.fromInputStream(io.in, serverLogger)
  val server =
    new LanguageServer(messages, client, myServices(serverLogger, client), scheduler, serverLogger)

  val connection = Connection(client, server.startTask.executeWithFork.runAsync)

  val evaluator = origEvaluator.copy(log = MultiLogger(colored = false, origEvaluator.log, new BspLogger))

  val rootModule = evaluator.rootModule

  // val evaluator = new Evaluator(ctx.home, pwd / 'out, pwd / 'out, rootModule, ctx.log)

  val completedOnExit: CompletableFuture[Unit] = new CompletableFuture

  def waitUntilExit(): Unit =
    completedOnExit.get()

  def buildTargetIdentifier(mod: Module): BuildTargetIdentifier = {
    val path = mod.millModuleSegments.render
    BuildTargetIdentifier(Uri(MillProtocol + path))
  }

  def buildTarget(mod: JavaModule): BuildTarget = {
    BuildTarget(
      id = buildTargetIdentifier(mod),
      displayName = Some(mod.toString),
      baseDirectory = Some(Uri(mod.millModuleBasePath.toString)),
//      kind = BuildTargetKind.Library,
      tags = ???, // TODO !missing-implementation!
      capabilities = BuildTargetCapabilities(canCompile = true, canTest = false, canRun = false),
      languageIds = List("scala"),
      dependencies = mod.moduleDeps.filter(isRepresentable).map(buildTargetIdentifier).toList,
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

  /** Is this module representable using BSP ? */
  def isRepresentable(mod: Module): Boolean =
    mod.isInstanceOf[ScalaModule]
    // Should be at least JavaModule, but not handled properly by IntelliJ

  def buildTargets(): List[BuildTarget] = {
    val modules = rootModule.millInternal.segmentsToModules.values
      .collect { case mod: JavaModule => mod }
      .filter(isRepresentable)

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

  def toBspUri(ref: PathRef): Uri = toBspUri(ref.path)
  def toBspUri(path: Path): Uri =
    Uri(path.toNIO.toUri.toString)

  def scalaConfig(target: BuildTargetIdentifier) = {
    val mod = parseId(target).asInstanceOf[ScalaModule]
    val (scalacOptions: Seq[String]) +: (compileClasspath: Agg[PathRef]) +: Nil =
      evaluator.evaluate(Agg(mod.scalacOptions, mod.compileClasspath)).values

    val paths = Evaluator.resolveDestPaths(evaluator.outPath, mod.compile.ctx.segments)

    ScalacOptionsItem(
      target,
      scalacOptions.toList,
      compileClasspath.map(toBspUri).toList,
      toBspUri(paths.dest)
    )
  }

  def sources(target: BuildTargetIdentifier) = {
    val mod = parseId(target).asInstanceOf[ScalaModule]
    val (sources: Seq[PathRef]) +: Nil =
      evaluator.evaluate(Agg(mod.sources)).values
    DependencySourcesItem(
      target,
      sources.map(toBspUri).toList
    )
  }


  def myServices(logger: LoggerSupport, client: LanguageClient): Services = {
    Services
      .empty(logger)
      .request(Build.initialize) { params =>
        logger.info(params.toString)
        InitializeBuildResult(BuildServerCapabilities(
          compileProvider = Some(CompileProvider(List("scala"))),
          testProvider = Some(TestProvider(Nil)),
          runProvider = Some(RunProvider(Nil)),
          inverseSourcesProvider = Some(false), // TODO !missing-implementation!,
//          textDocumentBuildTargetsProvider = false,
          dependencySourcesProvider = Some(false),
          resourcesProvider = Some(false),
          buildTargetChangedProvider = Some(false)
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
        completedOnExit.complete(())
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
        CompileResult(
          originId = originId,
          statusCode = StatusCode.Ok, // TODO !best-guess!
          data = None
        )
      }
      .request(BuildTargetEndpoint.dependencySources) { case DependencySourcesParams(targets) =>
        // sources of the actual project for IntelliJ
        println("t: " + targets)
        val d = DependencySourcesResult(targets.map(sources))
        // println("d: " + d)
        logger.info("d: " + d)
        d

        // DependencySourcesResult(targets.map(DependencySourcesItem(_, Nil)))
      }
      .request(BuildTargetEndpoint.scalacOptions) { case ScalacOptionsParams(targets) =>
        ScalacOptionsResult(targets.map(scalaConfig))
      }
      // DEBUG
      .notification(Build.publishDiagnostics) { params =>
        println("publishDiagnostics: " + params)
      }
  }

  def run() = {
    // val io = new InputOutput(System.in, System.out)

    // val clientLogger = Logger("client-logger").withHandler(writer = scribe.writer.FileWriter.simple())


    val res = (for {
      // init <- Build.initialize.request(InitializeBuildParams(Uri("file:///bla"), BuildClientCapabilities(languageIds = List("scala"), providesFileWatching = false)))
      // _ <- Task.now(println("init: " + init))
      // _ <- Task.fromFuture(Build.initialized.notify(InitializedBuildParams()))

      targets <- Workspace.buildTargets.request(WorkspaceBuildTargetsRequest())
      _ <- Task.now(println("targets: " + targets))
      // _ <- BuildTargetEndpoint.compile.request(CompileParams(targets.right.get.targets.map(_.id), Some("0"), Nil))
      _ <- BuildTargetEndpoint.dependencySources.request(DependencySourcesParams(targets.right.get.targets.map(_.id)))

      // _ <- Build.shutdown.request(Shutdown())
    } yield ()).runAsync

    scala.concurrent.Await.result(
      res,
      scala.concurrent.duration.Duration.Inf
    )
  }
}

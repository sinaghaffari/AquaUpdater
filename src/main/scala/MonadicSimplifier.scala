import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.implicitConversions
import scala.util.{Failure, Success}

object MonadicSimplifier {

  class FutureEitherOps[A](futureEither: Future[Either[Throwable, A]]) {
    def ?| : Step[A] = Step(futureEither)
  }
  class FutureOps[A](future: Future[A]) {
    def ?|(implicit executionContext: ExecutionContext): Step[A] = {
      val promise = Promise[Either[Throwable, A]]()
      future.onComplete {
        case Success(v) => promise.success(Right(v))
        case Failure(e) => promise.success(Left(e))
      }
      Step(promise.future)
    }
  }
  class OptionOps[A](option: Option[A]) {
    def ?|(ex: Throwable): Step[A] = Step(Future.successful(option.toRight(ex)))
  }
  class FutureOptionOps[A](futureOption: Future[Option[A]]) {
    def ?|(ex: Throwable)(implicit executionContext: ExecutionContext): Step[A] = Step((for {
      f <- new FutureOps(futureOption).?|
      o <- f.?|(ex)
    } yield o).run)
  }
  class EitherOps[A](either: Either[Throwable, A]) {
    def ?| : Step[A] = Step(Future.successful(either))
  }


  final case class Step[+A](run: Future[Either[Throwable, A]]) {
    def map[B](f: A => B)(implicit ec: ExecutionContext): Step[B] =
      copy(run = run.map(_.map(f)))

    def flatMap[B](f: A => Step[B])(implicit ec: ExecutionContext): Step[B] =
      copy(run = run.flatMap(_.fold(
        err => Future.successful(Left[Throwable, B](err)),
        succ => f(succ).run
      )))

    def withFilter(p: A => Boolean)(implicit ec: ExecutionContext): Step[A] =
      copy(run = run.filter {
        case Right(a) if p(a) => true
        case Left(e) => true
        case _ => false
      })
  }

  implicit def simplifyFutureEither[A](futureEither: Future[Either[Throwable, A]]): FutureEitherOps[A] = new
      FutureEitherOps(futureEither)
  implicit def simplifyFuture[A](future: Future[A]): FutureOps[A] = new FutureOps(future)
  implicit def simplifyOption[A](option: Option[A]): OptionOps[A] = new OptionOps(option)
  implicit def simplifyFutureOption[A](futureOption: Future[Option[A]]): FutureOptionOps[A] = new FutureOptionOps(futureOption)
  implicit def simplifyEither[A](either: Either[Throwable, A]): EitherOps[A] = new EitherOps(either)
}
package example

trait PipelineOperator [-T, +U] {
  def |> (data: T): Option[U]
}

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}

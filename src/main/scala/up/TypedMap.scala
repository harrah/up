package up


// a basic map that allows type-safe heterogeneous mapping
abstract class TypeMap {
  private[this] val backed = new collection.mutable.HashMap[Any, Any]()

  // represents a defined mapping from A -> B
  // subclasses should create new instances for each mapping that is allowed
  abstract class TypeMapping[A, B] protected()

  def update[I, O](i: I, out: O)(implicit ev: TypeMapping[I, O]) { backed.put((i,ev), out) }
  def apply[I, O](i: I)(implicit ev: TypeMapping[I, O]): O = backed((i,ev)).asInstanceOf[O]
  def getOr[I, O](i: I, or: => O)(implicit ev: TypeMapping[I, O]): O =
    backed.getOrElse((i,ev), or).asInstanceOf[O]
}

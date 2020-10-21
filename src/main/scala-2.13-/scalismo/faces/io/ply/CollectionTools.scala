package scalismo.faces.io.ply

object CollectionTools {

  def zip3[A,B,C](a: Seq[A], b: Seq[B], c: Seq[C]): Iterable[(A,B,C)] = {
    (a,b,c).zipped.toIterable
  }

}

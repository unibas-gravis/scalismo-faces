package scalismo.faces.image

case class LabeledPixelImage[A](image: PixelImage[A], label: PixelImage[Int]) {
  require(image.domain == label.domain, "LabeledPixelImage: image and mask must be comparable! (different sizes)")
}

package net.mbuzdalov.proteins

import java.awt.{Color, Font, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import java.util.Locale
import javax.imageio.ImageIO

object Heatmaps:
  def draw(cont: Container, proteins: Seq[String], target: String): Unit =
    val width = 750
    val height = 500
    val stripeOff = 40
    val stripeW = 30
    val fontSize = (width - height) / 6

    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_BGR)
    val g = img.createGraphics()

    val mx = Array.tabulate(proteins.size, proteins.size):
      (i, j) => cont.manhattanDistance(cont.index(proteins(i)), cont.index(proteins(j)))

    val max = mx.map(_.max).max
    Loops.foreach(0, mx.length)(i => mx(i)(i) = Double.PositiveInfinity)
    val min = mx.map(_.min).min

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, width, height)
    g.setRenderingHints(new RenderingHints(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON))

    val cellSize = height / proteins.size
    for x <- proteins.indices do
      for y <- proteins.indices do
        if x == y then
          g.setColor(Color.WHITE)
          g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
        else
          val d = mx(x)(y)
          val scaled = (d - min) / (max - min)
          g.setColor(new Color(Viridis(scaled)))
          g.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)

    for y <- 50 until height - 50 do
      val color = Viridis((y - 50).toDouble / (height - 100))
      for x <- 0 to stripeW do
        img.setRGB(x + height + stripeOff, y, color)

    val minStr = "%.03f".formatLocal(Locale.US, min)
    val maxStr = "%.03f".formatLocal(Locale.US, max)
    g.setColor(Color.BLACK)
    g.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, fontSize))
    g.drawString(minStr, height + stripeW + stripeOff + fontSize / 5, 50 + fontSize / 3)
    g.drawString(maxStr, height + stripeW + stripeOff + fontSize / 5, height - 50 + fontSize / 3)

    ImageIO.write(img, "png", new File(target))

end Heatmaps

package coop.rchain.caspersimulation.visualization

import java.awt.Color

import org.gephi.graph.api.{Element, Node}

object GephiUtil {
  def setElementColorByAttribute[T](elements: Iterator[Element], name: String, map: (T) => Color): Unit = {
    elements.foreach(e => {
      val a = e.getAttribute(name).asInstanceOf[T]
      e.setColor(map(a))
    })
  }

  def setNodeSizeByAttribute[T](nodes: Iterator[Node], name: String, map: (T) => Float): Unit = {
    nodes.foreach(n => {
      val a = n.getAttribute(name).asInstanceOf[T]
      n.setSize(map(a))
    })
  }

  def linearRange(sourceMin: Float,
                          sourceMax: Float,
                          targetMin: Float,
                          targetMax: Float): (Float) => Float = {
    val m = (targetMax - targetMin) / (sourceMax - sourceMin)

    (x: Float) => {m * (x - sourceMin) + targetMin}
  }

  val attributeNodeSize: Float = 2f

}

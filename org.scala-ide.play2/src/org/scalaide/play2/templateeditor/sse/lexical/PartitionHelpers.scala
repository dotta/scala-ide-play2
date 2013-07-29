package org.scalaide.play2.templateeditor.sse.lexical

import org.eclipse.jface.text.ITypedRegion
import org.eclipse.jface.text.TypedRegion
import org.scalaide.play2.templateeditor.lexical.TemplatePartitions
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

object PartitionHelpers {

  def isMagicAt(token: ITypedRegion, codeString: String) = {
    def tokenText = textOf(token, codeString).trim
    token.getType() == TemplatePartitions.TEMPLATE_DEFAULT && tokenText == "@"
  }
  
  def isBrace(token: ITypedRegion, codeString: String) = {
    def tokenText = textOf(token, codeString).trim
    token.getType() == TemplatePartitions.TEMPLATE_DEFAULT && tokenText.foldLeft(false)((r, c) => r && (c == '{' || c == '}' || c.isWhitespace))
  }

  private def textOf(token: ITypedRegion, documentContent: String): String = 
    documentContent.substring(token.getOffset(), token.getOffset() + token.getLength())

  private def detectSequence(codeString: String, fs: List[Char => Boolean], marked: Int, isIgnored: Char => Boolean): Option[Int] = {
    val fsMutable = fs.toBuffer
    var sawBadChar = false
    var index = -1
    var currentF = 0
    for (i <- 0 until codeString.length()) {
      val c = codeString(i)
      if (fsMutable.isEmpty) 
        sawBadChar = sawBadChar || !isIgnored(c)
      else {
        if (fsMutable.head(c)) {
          if (currentF == marked)
            index = i
          fsMutable.remove(0)
          currentF += 1
        }
        else
          sawBadChar = sawBadChar || !isIgnored(c)
      }
    }
    
    if(fsMutable.isEmpty && !sawBadChar)
      Some(index)
    else
      None
  }

  /**
   * Check if the given token has the following characteristics:
   *  - Represents a "Template default" partition.
   *  - Corresponding text matches the following pseudo-regex: (spaceOrTab?)('{' | '}')(spaceOrTab?)('@')(spaceOrTab?)
   */
  def isCombinedBraceMagicAt(token: ITypedRegion, codeString: String): Boolean = {
    def tokenText = textOf(token, codeString)
    if(token.getType() == TemplatePartitions.TEMPLATE_DEFAULT) {
      val isBrace = {c: Char => c == '{' || c == '}'}
      val isAt = {c: Char => c == '@'}
      val ignored = {c: Char => c == ' ' || c == '\t'}
      detectSequence(tokenText, List(isBrace, isAt), 0, ignored).isDefined
    }
    else false
  }

  /* Combines neighbouring regions based on some user provided criteria */
  def explodeAdjacent[T](partitions: Seq[ITypedRegion])(exploder: (ITypedRegion, ITypedRegion, T) => Seq[ITypedRegion])(test: (ITypedRegion, ITypedRegion) => Option[T]): IndexedSeq[ITypedRegion] = {
    val accum = new ArrayBuffer[ITypedRegion]
    for (region <- partitions) {
      if (accum.isEmpty)
        accum += region
      else {
        val previousRegion = accum.last
        test(previousRegion, region) match {
          case Some(tpe) => {
            accum.remove(accum.length - 1)
            accum.insert(accum.length, exploder(previousRegion, region, tpe):_*)
          }
          case None =>
            accum += region
        }
      }
    }
    accum
  }

  private def merge(l: ITypedRegion, r: ITypedRegion, t: String): List[TypedRegion] =
    List(new TypedRegion(l.getOffset, l.getLength + r.getLength, t))
    
  private val htmlPartitions: Set[String] = Set(TemplatePartitions.TEMPLATE_PLAIN, TemplatePartitions.TEMPLATE_TAG)
  
  /* Combines neighbouring regions that have the same type */
  def mergeAdjacentWithSameType(partitions: Seq[ITypedRegion]): IndexedSeq[ITypedRegion] = {
    explodeAdjacent(partitions)(merge) { (previousRegion, region) =>
      if (((htmlPartitions contains region.getType) && (htmlPartitions contains previousRegion.getType)) ||
         (region.getType == TemplatePartitions.TEMPLATE_SCALA && previousRegion.getType == TemplatePartitions.TEMPLATE_SCALA))
        Some(region.getType())
      else None
    }
  }

  /* Combine magic at with scala code partitions */
  def combineMagicAt(partitions: Seq[ITypedRegion], codeString: String): IndexedSeq[ITypedRegion] = {
    explodeAdjacent(partitions)(merge) { (left, right) =>
      if ((isMagicAt(left, codeString) && right.getType() == TemplatePartitions.TEMPLATE_SCALA) ||
          (left.getType() == TemplatePartitions.TEMPLATE_SCALA && isMagicAt(right, codeString)))
        Some(TemplatePartitions.TEMPLATE_SCALA)
      else None
    }
  }
  
  def separateBraceOrMagicAtFromEqual(partitions: Seq[ITypedRegion], codeString: String): IndexedSeq[ITypedRegion] = {
    def explode(l: ITypedRegion, r: ITypedRegion, splitIndex: Int): Seq[ITypedRegion] = {
      val first = new TypedRegion(l.getOffset(), splitIndex - l.getOffset(), l.getType())
      val second = new TypedRegion(splitIndex, l.getLength() - first.getLength(), l.getType())
      Seq(first, second, r)
    }
    
    explodeAdjacent(partitions)(explode) { (left, _) =>
      if (left.getType() == TemplatePartitions.TEMPLATE_DEFAULT) {
        def code = textOf(left, codeString)
        def isEquals = {c: Char => c == '='}
        def isBraceOrAt = {c: Char => c == '{' || c == '@'}
        def ignored = {c: Char => c == ' ' || c == '\t'}
        detectSequence(code, List(isEquals, isBraceOrAt), 1, ignored) map (_ + left.getOffset())
      }
      else None
    }
  }
}
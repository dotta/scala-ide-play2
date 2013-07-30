package org.scalaide.play2.templateeditor.sse.lexical

import java.io.Reader
import java.io.StringReader
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.tools.eclipse.lexical.ScalaCodeScanner
import scala.tools.eclipse.logging.HasLogger
import org.eclipse.jface.text.IDocument
import org.eclipse.ui.editors.text.EditorsUI
import org.eclipse.ui.texteditor.ChainedPreferenceStore
import org.eclipse.wst.sse.core.internal.ltk.parser.BlockMarker
import org.eclipse.wst.sse.core.internal.ltk.parser.RegionParser
import org.eclipse.wst.sse.core.internal.parser.ContextRegion
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocumentRegion
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion
import org.eclipse.wst.sse.core.internal.text.BasicStructuredDocumentRegion
import org.eclipse.wst.sse.core.internal.text.TextRegionListImpl
import org.eclipse.wst.xml.core.internal.parser.XMLSourceParser
import org.eclipse.wst.xml.core.internal.parser.regions.AttributeEqualsRegion
import org.eclipse.wst.xml.core.internal.parser.regions.AttributeNameRegion
import org.eclipse.wst.xml.core.internal.parser.regions.AttributeValueRegion
import org.eclipse.wst.xml.core.internal.parser.regions.EmptyTagCloseRegion
import org.eclipse.wst.xml.core.internal.parser.regions.EndTagOpenRegion
import org.eclipse.wst.xml.core.internal.parser.regions.TagCloseRegion
import org.eclipse.wst.xml.core.internal.parser.regions.TagNameRegion
import org.eclipse.wst.xml.core.internal.parser.regions.TagOpenRegion
import org.eclipse.wst.xml.core.internal.parser.regions.WhiteSpaceOnlyRegion
import org.eclipse.wst.xml.core.internal.parser.regions.XMLCDataTextRegion
import org.eclipse.wst.xml.core.internal.parser.regions.XMLContentRegion
import org.eclipse.wst.xml.core.internal.regions.DOMRegionContext
import org.scalaide.play2.PlayPlugin
import org.scalaide.play2.templateeditor.TemplateSyntaxClasses
import org.scalaide.play2.templateeditor.lexical.TemplatePartitionTokeniser
import org.scalaide.play2.templateeditor.lexical.TemplatePartitions
import scalariform.ScalaVersions
import scala.collection.mutable.HashMap
import org.eclipse.wst.xml.core.internal.parser.regions.AttributeNameRegion
import org.eclipse.wst.sse.core.internal.provisional.events.StructuredDocumentEvent
import org.eclipse.wst.sse.core.internal.provisional.events.RegionChangedEvent
import org.eclipse.wst.sse.core.internal.util.Debug
import org.eclipse.wst.xml.core.internal.parser.regions.RegionUpdateRule
import org.eclipse.wst.sse.core.internal.util.Utilities
import play.templates.ScalaTemplateParser
import org.eclipse.jface.text.TypedRegion
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import PartitionHelpers._
import org.eclipse.jface.text.ITypedRegion
import java.util.{ List => JList }
import java.util.{ ArrayList => JArrayList }
import org.eclipse.jdt.internal.core.util.SimpleDocument

object TemplateDocumentRegions {
  final val SCALA_DOC_REGION = "SCALA_CONTENT"
  final val COMMENT_DOC_REGION = "TEMPLATE_COMMENT"
  final val UNDEFINED = "UNDEFINED"
}

class TemplateRegionParser extends RegionParser with HasLogger {

  private[lexical] var templateRegions = new TemplateTextRegionsComputer("")
  override def newInstance: RegionParser = new TemplateRegionParser

  override def getDocumentRegions(): IStructuredDocumentRegion = templateRegions.structuredRegions.head

  /* Get the full list of known regions */
  override def getRegions(): JList[ITextRegion] = templateRegions.textRegions

  override def reset(input: String) =
    reset(new StringReader(input))

  override def reset(input: String, offset: Int) =
    reset(new StringReader(input), offset)

  override def reset(reader: Reader) = reset(reader, 0)

  override def reset(reader: Reader, offset: Int) = {
    val sb = new StringBuffer
    var c = reader.read()
    while (c != -1) {
      sb.append(c.toChar)
      c = reader.read()
    }
    templateRegions = new TemplateTextRegionsComputer(sb.toString())
  }
}

class TemplateTextRegionsComputer(documentContent: String) extends HasLogger {

  /** The tokens for this `documentContent`.
   *
   *  @note The standard Play Template parser doesn't tokenize the `documentContent` in exactly the way we expect it. 
   *        Hence, here we are mapping the tokens returned by the Play Template parse into the ones that fits our needs.
   */
  private lazy val tokens: Seq[ITypedRegion] = {
    val tokenizer = new TemplatePartitionTokeniser
    val originalTokens = tokenizer.tokenise(documentContent)
    separateBraceOrMagicAtFromEqual(originalTokens, documentContent)
  }

  /** Returns the `IStructuredDocumentRegion`s for this `documentContent`. */
  private[lexical] val structuredRegions: Array[IStructuredDocumentRegion] = {
    lazy val token2template = new TemplateTextRegionConverter(documentContent, tokens)

    // merge '@' tokens with scala code tokens, and then merge any adjacent tokens of the same type.
    // Merging here has the affect of not having neighbouring document regions of the same type
    val mergedTokens = mergeAdjacentWithSameType(combineMagicAt(tokens, documentContent))

    var resultRegions: IStructuredDocumentRegion = htmlHeadRegion

    for {
      token <- mergedTokens
      if !isHtmlToken(token)
    } {
      val (templateTextRegions: Seq[TemplateTextRegion], scalaDocType: String) = token2template(token)
      // If there are scala text regions, insert them into the HTML document regions
      if (templateTextRegions.nonEmpty) {
        // absolute offsets
        val (startEffectedOffset, endEffectedOffset) = (token.getOffset(), token.getOffset() + token.getLength())
        (htmlHeadRegion.regionMap.get(startEffectedOffset), htmlHeadRegion.regionMap.get(endEffectedOffset - 1)) match {
          case (Some(startEffectedDocumentRegion), Some(endEffectedDocumentRegion)) => {
            // If the scala text regions spans exactly the same space as the document regions it overlaps
            // Then replace those document regions with our own document regions
            if (startEffectedDocumentRegion.getStart() == startEffectedOffset && endEffectedDocumentRegion.getEnd() == endEffectedOffset) {
              val newRegion = replaceDocumentRegionsWithTemplateTextRegions(startEffectedDocumentRegion, endEffectedDocumentRegion, scalaDocType, templateTextRegions)
              if (htmlHeadRegion.regionCount == 1)
                resultRegions = newRegion
            }
            else {
              // handle the case where we need to trim text regions and then insert our text regions
              // Note: this code does not try to handle scala code that spans two (or more) xml document regions
              if (startEffectedDocumentRegion == endEffectedDocumentRegion)
                insertScalaRegions(startEffectedDocumentRegion, templateTextRegions, startEffectedOffset, endEffectedOffset)
              else
                insertScalaRegionsOverMultipleDocRegions(token.getOffset(), templateTextRegions, startEffectedDocumentRegion, startEffectedOffset, endEffectedDocumentRegion, endEffectedOffset)
            }
          }

          case _ => logger.debug("TemplateRegionParser: startEffectedDocumentRegion or endEffectedDocumentRegion could not be found.")
        }
      }
    }

    // The html parser returns null on empty input.. Talk about handling corner cases :P
    if (resultRegions == null) {
      resultRegions = new BasicStructuredDocumentRegion
      resultRegions.setStart(0)
      resultRegions.setLength(documentContent.length())
      resultRegions.addRegion(new ContextRegion(TemplateDocumentRegions.UNDEFINED, 0, documentContent.length(), documentContent.length()))
    }

    val ab = new ArrayBuffer[IStructuredDocumentRegion]
    var current = resultRegions
    while (current != null) {
      ab += current
      current = current.getNext()
    }

    ab.result.toArray
  }

  /** Returns the `ITextRegion`s for this `documentContent`. */
  lazy val textRegions: JList[ITextRegion] = {
    val resultList = new JArrayList[ITextRegion]()
    structuredRegions.foreach(dr => {
      for (textRegion: ITextRegion <- dr.getRegions().toArray) {
        resultList.add(textRegion)
      }
    })
    resultList
  }

  private val htmlHeadRegion: RichStructuredDocumentRegion = {
    // The block regions enable javascript and css support, as BLOCK_TEXT regions are treated special by the html component
    import org.eclipse.wst.xml.core.internal.regions.DOMRegionContext
    import org.eclipse.wst.sse.core.internal.ltk.parser.BlockMarker
    val htmlParser = new XMLSourceParser
    htmlParser.addBlockMarker(new BlockMarker("script", null, DOMRegionContext.BLOCK_TEXT, false))
    htmlParser.addBlockMarker(new BlockMarker("style", null, DOMRegionContext.BLOCK_TEXT, false))
    htmlParser.reset(documentContent)
    htmlParser.getDocumentRegions()
  }

  private def isHtmlToken(token: ITypedRegion): Boolean = {
    token.getType() == TemplatePartitions.TEMPLATE_PLAIN ||
    token.getType() == TemplatePartitions.TEMPLATE_TAG ||
    (token.getType() == TemplatePartitions.TEMPLATE_DEFAULT && !PartitionHelpers.isBrace(token, documentContent) && !PartitionHelpers.isCombinedBraceMagicAt(token, documentContent))
  }

  private def replaceDocumentRegionsWithTemplateTextRegions(leftReplacedDocumentRegion: IStructuredDocumentRegion, rightReplacedDocumentRegion: IStructuredDocumentRegion, templateDocType: String, templateTextRegions: Seq[TemplateTextRegion]): IStructuredDocumentRegion = {
    val startEffectedOffset = leftReplacedDocumentRegion.getStart()
    val endEffectedOffset = rightReplacedDocumentRegion.getEnd()
    val region = new BasicStructuredDocumentRegion { override def getType() = templateDocType }
    region.setStart(startEffectedOffset)
    region.setLength(endEffectedOffset - startEffectedOffset)
    templateTextRegions.foreach(region.addRegion(_))
    // insert our new region where the effected regions were
    Option(leftReplacedDocumentRegion.getPrevious()).foreach { previous =>
      previous.setNext(region)
      region.setPrevious(previous)
    }
    Option(rightReplacedDocumentRegion.getNext()) match {
      case Some(next) =>
        next.setPrevious(region)
        region.setNext(next)
      case None => region.setEnded(true)
    }
    region
  }

  private[lexical] implicit def richStructuredDocumentRegion2structuredDocumentRegion(doc: RichStructuredDocumentRegion): IStructuredDocumentRegion = doc.region
  private[lexical] implicit class RichStructuredDocumentRegion(val region: IStructuredDocumentRegion) {
    /* Maps document offsets to document regions */
    lazy val regionMap: collection.Map[Int, IStructuredDocumentRegion] = {
      val map = new HashMap[Int, IStructuredDocumentRegion]
      var current = region
      while (current != null) {
        for (i <- current.getStart() until current.getEnd())
          map(i) = current
        current = current.getNext()
      }
      map
    }

    /* How many structured regions are in linked list that this region is the head of? */
    lazy val regionCount = {
      @tailrec
      def aux(region: IStructuredDocumentRegion, count: Int): Int = {
        if (region == null) count
        else aux(region.getNext(), count + 1)
      }
      aux(region, 0)
    }
  }

  private def insertScalaRegions(effectedDocRegion: IStructuredDocumentRegion, templateTextRegions: Seq[TemplateTextRegion], startEffectedOffset: Int, endEffectedOffset: Int) = {
    val textRegions = effectedDocRegion.getRegions().toArray().toArray // "textregionlist to java array to scala array
    val leftTextRegion = effectedDocRegion.getRegionAtCharacterOffset(startEffectedOffset)
    val rightTextRegion = effectedDocRegion.getRegionAtCharacterOffset(endEffectedOffset - 1) // end offsets are not inclusive
    val newTextRegions = new ArrayBuffer[ITextRegion]

    // add text regions up to leftTextRegion
    var i = 0
    while (textRegions(i) != leftTextRegion) {
      newTextRegions += textRegions(i)
      i += 1
    }

    // split left text region if necessary
    if (effectedDocRegion.getStartOffset(leftTextRegion) < startEffectedOffset) {
      val leftSplit = copyXMLTextRegion(leftTextRegion)
      val adjustment = -(effectedDocRegion.getEndOffset(leftTextRegion) - startEffectedOffset)
      leftSplit.adjustLength(adjustment)
      leftSplit.adjustTextLength(adjustment)
      newTextRegions += leftSplit
    }

    // add template text regions
    var currentTemplateOffset = startEffectedOffset - effectedDocRegion.getStart()
    for (tr <- templateTextRegions) {
      tr.setStart(currentTemplateOffset)
      newTextRegions += tr
      currentTemplateOffset += tr.getLength()
    }

    // split right text region if necessary
    if (effectedDocRegion.getEndOffset(rightTextRegion) > endEffectedOffset) {
      val rightSplit = copyXMLTextRegion(rightTextRegion)
      val adjustment = endEffectedOffset - effectedDocRegion.getStartOffset(rightTextRegion)
      rightSplit.adjustStart(adjustment)
      rightSplit.adjustLength(-adjustment)
      rightSplit.adjustTextLength(-adjustment)
      newTextRegions += rightSplit
    }

    // add text regions after right text region until end
    i = 0
    while (i < textRegions.length) {
      val tr = textRegions(i)
      if (tr.getStart() > rightTextRegion.getStart()) {
        newTextRegions += tr
      }
      i += 1
    }

    // replace the doc's text regions with the new text regions
    val newTextRegionList = new TextRegionListImpl
    for (tr <- newTextRegions)
      newTextRegionList.add(tr)
    effectedDocRegion.setRegions(newTextRegionList)
  }

  private def copyXMLTextRegion(region: ITextRegion): ITextRegion = region match {
    case attribEquals: AttributeEqualsRegion => new AttributeEqualsRegion(attribEquals.getStart(), attribEquals.getTextLength(), attribEquals.getLength())
    case attribName: AttributeNameRegion     => new FixedAttributeNameRegion(attribName.getStart(), attribName.getTextLength(), attribName.getLength())
    case attribValue: AttributeValueRegion   => new AttributeValueRegion(attribValue.getStart(), attribValue.getTextLength(), attribValue.getLength())
    case emptyTagClose: EmptyTagCloseRegion  => new EmptyTagCloseRegion(emptyTagClose.getStart(), emptyTagClose.getTextLength(), emptyTagClose.getLength())
    case endTagOpen: EndTagOpenRegion        => new EndTagOpenRegion(endTagOpen.getStart(), endTagOpen.getTextLength(), endTagOpen.getLength())
    case tagClose: TagCloseRegion            => new TagCloseRegion(tagClose.getStart())
    case tagName: TagNameRegion              => new TagNameRegion(tagName.getStart(), tagName.getTextLength(), tagName.getLength())
    case tagOpen: TagOpenRegion              => new TagOpenRegion(tagOpen.getStart(), tagOpen.getTextLength(), tagOpen.getLength())
    case whitespace: WhiteSpaceOnlyRegion    => new WhiteSpaceOnlyRegion(whitespace.getStart(), whitespace.getLength())
    case cdata: XMLCDataTextRegion           => new XMLCDataTextRegion(cdata.getStart(), cdata.getTextLength(), cdata.getLength())
    case content: XMLContentRegion           => new XMLContentRegion(content.getStart(), content.getLength())
    case context: ContextRegion              => new ContextRegion(context.getType(), context.getStart(), context.getTextLength(), context.getLength())
    case _ => {
      logger.error(s"TemplateRegionParser: Unhandled attempt to copy XML region: $region")
      null
    }
  }

  private def insertScalaRegionsOverMultipleDocRegions(globalTokenOffset: Int, templateTextRegions: Seq[TemplateTextRegion], startEffectedDocumentRegion: IStructuredDocumentRegion, startEffectedOffset: Int, endEffectedDocumentRegion: IStructuredDocumentRegion, endEffectedOffset: Int) = {
    // Get a list of all effected document regions, plus their corresponding start and end effected offsets
    //   (which will correspond exactly to the document's start and end for the non-head and non-tail elements.
    var effectedDocuments: ListBuffer[(IStructuredDocumentRegion, Int, Int)] = overlappedDocumentRegions(startEffectedDocumentRegion, endEffectedDocumentRegion, startEffectedOffset, endEffectedOffset, true)
    var currentTextRegion = 0
    val scalaRegions: ArrayBuffer[TemplateTextRegion] = ArrayBuffer()
    for ((effectedDocRegion, startEffectedOffset, endEffectedOffset) <- effectedDocuments) {
      def globalOffset(offset: Int) = globalTokenOffset + offset
      def copyScalaRegion(tr: TemplateTextRegion, newStart: Option[Int] = None, newLength: Option[Int] = None): TemplateTextRegion = {
        val trcopy = tr.copy()
        newStart.foreach(trcopy.setStart(_))
        newLength.foreach(trcopy.setLength(_))
        trcopy
      }

      var done = false
      while (!done && currentTextRegion < templateTextRegions.length) {
        val tr = templateTextRegions(currentTextRegion)
        val trGlobalStart = globalOffset(tr.getStart())
        val trGlobalEnd = globalOffset(tr.getEnd())

        val contained = trGlobalStart >= startEffectedOffset && trGlobalEnd <= endEffectedOffset
        val leftOverlap = trGlobalStart < startEffectedOffset && trGlobalEnd <= endEffectedOffset
        val rightOverlap = trGlobalStart >= startEffectedOffset && trGlobalStart < endEffectedOffset && trGlobalEnd > endEffectedOffset
        val overspan = trGlobalStart <= startEffectedOffset && trGlobalEnd >= endEffectedOffset

        // the scala text region is fully within the doc region
        if (contained) {
          scalaRegions += tr
          currentTextRegion += 1
        }
        // the scala text region overlaps the left hand side of the doc region
        else if (leftOverlap) {
          scalaRegions += copyScalaRegion(tr, Some(startEffectedOffset - effectedDocRegion.getStart()), Some(tr.getLength() - (startEffectedOffset - trGlobalStart)))
          currentTextRegion += 1
        }
        // the scala text region overlaps the right hand side of the doc region
        else if (rightOverlap) {
          scalaRegions += copyScalaRegion(tr, newLength = Some(tr.getLength() - (trGlobalEnd - endEffectedOffset)))
          // don't increment currentTextRegion because it might also be used by the next structured document
        }
        // the scala text region fully encompasses the doc region 
        else if (overspan) {
          scalaRegions += copyScalaRegion(tr, Some(0), Some(effectedDocRegion.getLength()))
          // don't increment currentTextRegion because it might also be used by the next structured document
        }

        done = overspan || rightOverlap || (trGlobalStart >= endEffectedOffset)
      }

      insertScalaRegions(effectedDocRegion, scalaRegions, startEffectedOffset, endEffectedOffset)
      scalaRegions.clear()
    }
  }

  private def overlappedDocumentRegions(leftBound: IStructuredDocumentRegion, rightBound: IStructuredDocumentRegion, startOffset: Int, endOffset: Int, fillGaps: Boolean): ListBuffer[(IStructuredDocumentRegion, Int, Int)] = {
    val result: ListBuffer[(IStructuredDocumentRegion, Int, Int)] = ListBuffer((leftBound, startOffset, leftBound.getEnd()))
    var doc = leftBound.getNext()
    while (doc != rightBound) {
      doc = doc.getNext()
      if (doc != rightBound)
        result += ((doc, doc.getStart(), doc.getEnd()))
    }
    result += ((rightBound, rightBound.getStart(), endOffset))

    // there can be gaps.. so create new doc regions for the gaps
    if (fillGaps) {
      for (i <- 0 to (result.length - 2)) {
        val ((l, lstart, lend), (r, rstart, _)) = result(i) -> result(i + 1)
        if (lend != rstart) {
          val newRegion = new BasicStructuredDocumentRegion
          newRegion.addRegion(new ContextRegion("UNDEFINED", 0, rstart - lend, rstart - lend))
          newRegion.setStart(lend)
          newRegion.setLength(rstart - lend)
          l.setNext(newRegion)
          newRegion.setPrevious(l)
          newRegion.setNext(r)
          r.setPrevious(newRegion)
          result.insert(i + 1, (newRegion, lend, rstart))
        }
      }
    }

    result
  }
}

/** Converts the passed `tokens` into `TemplateTextRegion`.
 *
 *  @param documentContent The document's content.
 *  @param tokens The `tokens` for the passed `documentContent`.
 */
private class TemplateTextRegionConverter(documentContent: String, tokens: Seq[ITypedRegion]) {

  private val tokenIndexLookup: mutable.HashMap[Int, ITypedRegion] = {
    def createTokenIndexLookup(tokens: Seq[ITypedRegion]): mutable.HashMap[Int, ITypedRegion] = {
      val tokenIndexLookup = new mutable.HashMap[Int, ITypedRegion]
      tokenIndexLookup.sizeHint(tokens.length)
      for (reg <- tokens) {
        val kv = (reg.getOffset(), reg)
        tokenIndexLookup += kv
      }
      tokenIndexLookup
    }
    
    createTokenIndexLookup(tokens)
  }

  def apply(token: ITypedRegion): (Seq[TemplateTextRegion], String) = {
    if (token.getType == TemplatePartitions.TEMPLATE_SCALA) {
      computeScalaRegions(token)
    }
    else if (PartitionHelpers.isBrace(token, documentContent))
      (List(new TemplateTextRegion(TemplateSyntaxClasses.BRACE, 0, token.getLength(), token.getLength())), TemplateDocumentRegions.SCALA_DOC_REGION)
    else if (token.getType == TemplatePartitions.TEMPLATE_COMMENT)
      (List(new TemplateTextRegion(TemplateSyntaxClasses.COMMENT, 0, token.getLength(), token.getLength())), TemplateDocumentRegions.COMMENT_DOC_REGION)
    else if (PartitionHelpers.isCombinedBraceMagicAt(token, documentContent)) {
      def isBrace(c: Char ): Boolean = (c == '{' || c == '}')
      val splitIndex = documentContent.indexWhere(isBrace, token.getOffset())
      val braceLen = splitIndex - token.getOffset()
      val magicAtLen = token.getLength() - braceLen
      val brace = new TemplateTextRegion(TemplateSyntaxClasses.BRACE, 0, braceLen, braceLen)
      val magicAt = new TemplateTextRegion(TemplateSyntaxClasses.MAGIC_AT, braceLen, magicAtLen, magicAtLen)
      (List(brace, magicAt), TemplateDocumentRegions.SCALA_DOC_REGION)
    }
    else (Nil, TemplateDocumentRegions.UNDEFINED)
  }

  private def computeScalaRegions(token: ITypedRegion): (Seq[TemplateTextRegion], String) = {
    val regions = new ListBuffer[TemplateTextRegion]
    var currentIndex = token.getOffset()
    while (currentIndex < (token.getOffset() + token.getLength())) {
      val t = tokenIndexLookup(currentIndex)
      if (PartitionHelpers.isMagicAt(t, documentContent)) {
        regions += new TemplateTextRegion(TemplateSyntaxClasses.MAGIC_AT, t.getOffset() - token.getOffset(), t.getLength(), t.getLength())
      } // actual scala code
      else { //if (t.getType() == TemplatePartitions.TEMPLATE_SCALA) {
        // TODO - figure out a good way to get the prefstore from the editor
        val prefStore = new ChainedPreferenceStore(Array((EditorsUI.getPreferenceStore()), PlayPlugin.preferenceStore))
        val scanner = new ScalaCodeScanner(prefStore, ScalaVersions.Scala_2_10)
        val doc = new SimpleDocument(documentContent)
        val tokens = scanner.tokenize(doc, t.getOffset(), t.getLength())
        tokens.foreach { v =>
          regions += new TemplateTextRegion(v.syntaxClass, v.start - token.getOffset(), v.length, v.length)
        }
      }
      currentIndex += t.getLength()
    }

    (regions, TemplateDocumentRegions.SCALA_DOC_REGION)
  }
}

// This is a copy and pasted implementation of AttributeNameRegion, with the only change being that adjustTextLength
// actually adds the parameter to the field :). Had to reimplement the whole class because the fields in AttributeNameRegion are private
class FixedAttributeNameRegion(start: Int, textLength: Int, length: Int) extends AttributeNameRegion(start, textLength, length) with HasLogger {
  private var fLength: Int = length
  private var fStart: Int = start
  private var fTextLength: Int = textLength

  override def adjustLength(i: Int) { fLength += i }
  override def adjustStart(i: Int) { fStart += i }
  override def adjustTextLength(i: Int) { fTextLength += i } // this is the only thing that's changed!!
  override def equatePositions(region: ITextRegion) {
    fStart = region.getStart
    fLength = region.getLength
    fTextLength = region.getTextLength
  }
  override def getEnd(): Int = fStart + fLength
  override def getLength(): Int = fLength
  override def getStart(): Int = fStart
  override def getTextEnd(): Int = fStart + fTextLength
  override def getTextLength(): Int = fTextLength
  override def updateRegion(requester: AnyRef,
    parent: IStructuredDocumentRegion,
    changes: String,
    requestStart: Int,
    lengthToReplace: Int): StructuredDocumentEvent = {
    var result: RegionChangedEvent = null
    if (Debug.debugStructuredDocument) {
      logger.debug("\t\tContextRegion::updateModel")
      logger.debug("\t\t\tregion type is " + getType())
    }
    var canHandle = false
    canHandle = if ((changes == null) || (changes.length == 0)) if ((fStart >= getTextEnd) ||
      (Math.abs(lengthToReplace) >= getTextEnd - getStart)) false else true
    else if ((RegionUpdateRule.canHandleAsWhiteSpace(this,
      parent, changes, requestStart, lengthToReplace)) ||
      RegionUpdateRule.canHandleAsLetterOrDigit(this, parent, changes, requestStart, lengthToReplace)) true else false
    if (canHandle) {
      if (Debug.debugStructuredDocument) {
        logger.debug("change handled by region")
      }
      val lengthDifference = Utilities.calculateLengthDifference(changes, lengthToReplace)
      if (!RegionUpdateRule.canHandleAsWhiteSpace(this, parent, changes, requestStart, lengthToReplace)) {
        fTextLength += lengthDifference
      }
      fLength += lengthDifference
      result = new RegionChangedEvent(parent.getParentDocument, requester, parent, this, changes, requestStart,
        lengthToReplace)
    }
    result
  }
}
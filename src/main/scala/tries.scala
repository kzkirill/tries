/**
  * Created by Kirill on 1/7/2017.
  */

import common.{parallel}

package object tries {
  type Dictionary = List[String]

  case class TrieNode(val text: String, val children: Map[Char, TrieNode], val isWord: Boolean) {

    def this() = this("", Map.empty, false)


    def combine(that: TrieNode): TrieNode = {
      val (same, thatChildren) = that.children.partition {
        case (key, _) => children.contains(key)
      }
      val combinedChildren = same.map {
        case (child, node) => child -> node.combine(that.children(child))
      }
      val mine = children filterKeys (k => !same.contains(k))
      TrieNode(text, mine ++ combinedChildren ++ thatChildren, isWord)
    }

    def insert(c: Char, isLast: Boolean): TrieNode = {
      children get (c) match {
        case None => {
          val newText = text + c
          TrieNode(text, children + (c -> TrieNode(newText, Map.empty, isLast)), isWord)
        }
        case Some(child) =>
          TrieNode(text, children - c + (c -> child.insert(c, isLast)), isWord)
      }
    }

    def predict(prefix: String, predictionsNumber: Int): Dictionary = {
      find(prefix) match {
        case None => Nil
        case Some(node) => {
          getCompletionPredictions(predictionsNumber, List(), List(node))
        }
      }
    }

    def find(prefix: String): Option[TrieNode] = {
      find(prefix, prefix.toList)
    }

    def find(prefix: String, nextKeys: List[Char]): Option[TrieNode] = {
      if (this.text equals prefix) Some {
        this
      }
      else {
        children get nextKeys.head match {
          case None => None
          case Some(next) => next.find(prefix, nextKeys.tail)
        }
      }
    }


    def getCompletionPredictions(required: Int, found: List[String], queue: List[TrieNode]): List[String] = {
      queue match {
        case Nil => found
        case popped :: tail =>
          val updatedFound = if (popped.isWord) popped.text :: found else found
          if (updatedFound.length == required) updatedFound
          else
            getCompletionPredictions(required, updatedFound, tail ++ popped.children.values)
      }
    }
  }

  def insertWord(node: TrieNode, word: List[Char]): TrieNode = {
    word match {
      case Nil => TrieNode(node.text, node.children, true)
      case c :: cs => {
        val withC = node.insert(c, false)
        val next = withC.children(c)
        TrieNode(node.text, node.children - c + (c -> insertWord(next, cs)), node.isWord)
      }
    }
  }

  def populate(node: TrieNode, words: Dictionary): TrieNode = {
    words match {
      case Nil => node
      case x :: xs => populate(insertWord(node, x.toList), xs)
    }
  }

  def loadPar(words: Array[String], from: Long, to: Long): TrieNode = {
    if ((to - from) > 2) {
      val middle = (to - from) / 2
      val firstQuarter = (middle - from) / 2
      val thirdQuarter = middle + (to - middle) / 2
      val (t1, t2, t3, t4) = parallel(loadPar(words, from, firstQuarter),
        loadPar(words, firstQuarter + 1, middle),
        loadPar(words, middle + 1, thirdQuarter),
        loadPar(words, thirdQuarter + 1, to))

      val (combined1, combined2) = parallel(t1.combine(t2), t3.combine(t4))
      combined1.combine(combined2)
    } else {
      populate(new TrieNode(), words.toList)
    }
  }


}
import tries.populate
import tries.loadPar
import tries.populate

import org.scalatest.FunSuite
import tries.TrieNode

/**
  * Created by Kirill on 10/22/2016.
  */
class TrieNodeTest extends FunSuite {
  def trieRoot = TrieNode("", Map.empty, false)

  def dictionary = List("eat", "ear", "beef", "born", "east", "eats")

  test("Creating Trie node") {
    val dict = populate(trieRoot, dictionary)
    println(dict)
  }

  test("Getting predictions") {
    val dict = populate(trieRoot, dictionary)

    val one = dict.predict("ea", 1)
    assert(one.contains("ear"), "Prediction of size 1 for ea should be ear")
    assert(!one.contains("eat"), "Prediction of size 1 for ea should contain only ear")

    val two = dict.predict("ea", 3)
    assert(two.contains("ear"), "Prediction of size 2 for ea should contain ear")
    assert(two.contains("eat"), "Prediction of size 2 for ea should contain eat")

    val forbeef = dict.predict("be", 3)
    assert(forbeef.contains("beef"), "Prediction of for bee should contain beef")
    assert(!forbeef.contains("be"), "Prediction of for bee should contain only legal words not be")

  }

  test("combine tries") {
    val beer = populate(trieRoot, List("beer"))
    val ear = populate(trieRoot, List("ear"))
    val east = populate(trieRoot, List("east"))
    val combined = ear.combine(east).combine(beer)
    println(combined)
    assert(!combined.predict("bee", 3).contains("bee"), "Prediction for bee should not contain bee")
    assert(combined.predict("bee", 3).contains("beer"), "Prediction for bee should contin beear")
    val predictEa = combined.predict("ea", 3)
    assert(predictEa.contains("east") && predictEa.contains("east"), "Prediction for ea should contain ear and east")
  }

  test("load in parallel") {
    val words = Array("ear", "bear", "beer", "earn", "beard", "east", "bee", "easter", "become", "eager", "boar", "board")

    val result = loadPar(words,0,words.length - 1)

    val predictBe4 = result.predict("be",4)
    assert(predictBe4.length === 4, "4 predictions for be should be of length 4 " + predictBe4)
    assert(predictBe4.contains("bee"),"4 predictions for be should contain bee")
    assert(predictBe4.contains("beer"),"4 predictions for be should contain beer")
    assert(predictBe4.contains("bear"),"4 predictions for be should contain bear")
    assert(predictBe4.contains("beard"),"4 predictions for be should contain beard")

    val predictBe2 = result.predict("be",2)
    assert(predictBe2.length === 2, "2 predictions for be should be of length 2 " + predictBe4)
    assert(predictBe2.contains("bee"),"2 predictions for be should contain bee")
    assert(predictBe2.contains("bear"),"2 predictions for be should contain bear")
    //    println(result)
//    println(predictBe)
  }
}

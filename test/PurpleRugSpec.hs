module PurpleRugSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import PurpleRug.Internal
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "headers" $ do
    it "converts markdown headers to html headers" $ do
      headers "### I love fish tacos" `shouldBe` "<h3>I love fish tacos</h3>"

    it "can convert headers of different sizes" $ do
      headers "##### I love fish tacos" `shouldBe` "<h5>I love fish tacos</h5>"

    it "does not go farther than 6 headers" $ do
      headers "######## I love fish tacos" `shouldBe` "######## I love fish tacos"

    it "returns input when input lacks whitespace buffer" $ do
      headers "###I love fish tacos" `shouldBe` "###I love fish tacos"

    it "returns input when given empty string" $ do
      headers "" `shouldBe` ""

    it "returns input when given input with no # signs" $ do
      headers "I love fish tacos" `shouldBe` "I love fish tacos"

    it "returns input when given only whitespace" $ do
      headers " " `shouldBe` " "

  describe "paragraphs" $ do
    it "can handle empty input" $ do
      paragraphs "" `shouldBe` ""

    it "converts single lines to paragraphs" $ do
      paragraphs "I love fish tacos." `shouldBe` "<p>I love fish tacos.</p>"

    it "keeps single line breaks within same p tags" $ do
      paragraphs "I love fish tacos.\nThey're really great!" `shouldBe` "<p>I love fish tacos. They're really great!</p>"

    it "places double line breaks into new paragraph, separated by line break" $ do
      paragraphs "I love fish tacos.\n\nThey're really great!" `shouldBe` "<p>I love fish tacos.</p>\n<p>They're really great!</p>"

    it "applies any additional line breaks between paragraphs" $ do
      paragraphs "I love fish tacos.\n\n\nThey're really great!" `shouldBe` "<p>I love fish tacos.</p>\n<p>They're really great!</p>"

    it "ignores additional newlines" $ do
      paragraphs "I love fish tacos.\n\n\n\n\n\nThey're really great!" `shouldBe` "<p>I love fish tacos.</p>\n<p>They're really great!</p>"

  describe "formatWords" $ do
    it "can handle an empty string" $ do
      formatWords "" `shouldBe` ""

    it "returns input when no emphases exist" $ do
      formatWords "I love fish tacos" `shouldBe` "I love fish tacos"

    it "converts words wrapped in '*' to <em> tags" $ do
      formatWords "I *love* fish tacos." `shouldBe` "I <em>love</em> fish tacos."

    it "converts separately emphasized words" $ do
      formatWords "I *love* fish *tacos*." `shouldBe` "I <em>love</em> fish <em>tacos</em>."

    it "converts multiple words" $ do
      formatWords "I *love fish* tacos." `shouldBe` "I <em>love fish</em> tacos."

    it "converts emphasis mid-word" $ do
      formatWords "I lo*ve fi*sh tacos." `shouldBe` "I lo<em>ve fi</em>sh tacos."

    it "converts words wrapped in '**' to <strong> tags" $ do
      formatWords "I **love** fish tacos." `shouldBe` "I <strong>love</strong> fish tacos."

    it "converts separately strong words" $ do
      formatWords "I **love** fish **tacos**." `shouldBe` "I <strong>love</strong> fish <strong>tacos</strong>."

    it "converts multiple strong words" $ do
      formatWords "I **love fish** tacos." `shouldBe` "I <strong>love fish</strong> tacos."

    it "converts strongs mid-word" $ do
      formatWords "I lo**ve fi**sh tacos." `shouldBe` "I lo<strong>ve fi</strong>sh tacos."

    it "converts em nested inside strong" $ do
      formatWords "**I *love* fish** tacos." `shouldBe` "<strong>I <em>love</em> fish</strong> tacos."

    it "converts strong nested inside em" $ do
      formatWords "*I **love** fish* tacos." `shouldBe` "<em>I <strong>love</strong> fish</em> tacos."

    it "converts a combination of both" $ do
      formatWords "I *lo**ve** fi*sh tacos." `shouldBe` "I <em>lo<strong>ve</strong> fi</em>sh tacos."

  -- describe "splitEvery" $ do
    -- it "splits at every element" $ do
    --   (splitEvery ". " "1. Fish\n2. Salmon\n3. Eggs") `shouldBe` ["1", "Fish", "2", "Salmon", "3", "Eggs"]

--     it "splits at every element" $ do
--       (splitEvery "* " "* Fish\n* Eggs\n* Salmon") `shouldBe` ["", "Fish\n", "Eggs\n", "Salmon"]

  describe "splitBy" $ do
    it "splits at every element that given predicate returns true for" $ do
      (splitBy isDigit "1 Fish\n2 Eggs\n3 Salmon") `shouldBe` [" Fish\n", " Eggs\n", " Salmon"]

  describe "unorderedLists" $ do
    it "converts unordered lists" $ do
      unorderedLists "* Fish\n* Eggs\n* Salmon" `shouldBe` "<ul>\n<li>Fish</li>\n<li>Eggs</li>\n<li>Salmon</li>\n</ul>"

    it "ignores list item when no buffer space between '*' and text" $ do
      unorderedLists "* Fish\n*Eggs\n* Salmon" `shouldBe` "<ul>\n<li>Fish\n*Eggs</li>\n<li>Salmon</li>\n</ul>"

    it "ignores excessive line breaks" $ do
      unorderedLists "* Fish\n\n\n\n\n* Eggs\n\n\n\n\n* Salmon" `shouldBe` "<ul>\n<li>Fish</li>\n<li>Eggs</li>\n<li>Salmon</li>\n</ul>"

  describe "orderedLists" $ do
    it "converts unordered lists" $ do
      orderedLists "1. Fish\n2. Eggs\n3. Salmon" `shouldBe` "<ol>\n<li>Fish</li>\n<li>Eggs</li>\n<li>Salmon</li>\n</ol>"

    -- it "requires newlines between list items" $ do
    --   orderedLists "1. Fish 2. Eggs 3. Salmon" `shouldBe` "<ol>\n<li>Fish 2. Eggs 3. Salmon</li>\n</ol>"

    -- it "ignores list item when no buffer space between '*' and text" $ do
    --   orderedLists "1. Fish\n2.Eggs\n3. Salmon" `shouldBe` "<ol>\n<li>Fish\n*Eggs</li>\n<li>Salmon</li>\n</ol>"

    -- it "ignores excessive line breaks" $ do
    --   orderedLists "1. Fish\n\n\n\n\n2. Eggs\n\n\n\n\n3. Salmon" `shouldBe` "<ol>\n<li>Fish\n*Eggs</li>\n<li>Salmon</li>\n</ol>"

    -- it "only converts when '.' follows number" $ do
    --   orderedLists "1. Fish\n2 Eggs\n3. Salmon" `shouldBe` "<ol>\n<li>Fish\n2 Eggs</li>\n<li>Salmon</li>\n</ol>"

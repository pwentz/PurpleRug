module InitializersSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.QuickCheck hiding (orderedList)

import Initializers.FormattedWords
import Initializers.Headers
import Initializers.OrderedList
import Initializers.Paragraphs
import Initializers.UnorderedList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "UnorderedList" $ do
        it "wraps an array of list items" $ do
            let (Just res) = unorderedList "* Fish\n* Eggs\n* Salmon\n* Cake"
            getUnorderedList res `shouldBe` ["Fish", "Eggs", "Salmon", "Cake"]
        it "can format list into html elements" $ do
            let (Just res) = unorderedList "* Fish\n* Eggs\n* Salmon\n* Cake"
            formatUnorderedList res `shouldBe`
                "<ul>\n<li>Fish</li>\n<li>Eggs</li>\n<li>Salmon</li>\n<li>Cake</li>\n</ul>"
        it "doesn't create list item for non-formatted items" $ do
            let (Just res) = unorderedList "* Fish\n*Eggs\n* Salmon"
            getUnorderedList res `shouldBe` ["Fish\n*Eggs", "Salmon"]
        it "doesn't create separate list items for inline items" $ do
            let (Just res) = unorderedList "* Fish * Eggs * Salmon"
            getUnorderedList res `shouldBe` ["Fish * Eggs * Salmon"]
        it "returns Nothing if input doesn't lead with '* '" $ do
            unorderedList "Fish\n* Eggs\n* Salmon" `shouldBe` Nothing
    describe "OrderedList" $ do
        it "wraps an array of list items" $ do
            let (Just res) = orderedList "1. Fish\n2. Eggs\n3. Salmon"
            getOrderedList res `shouldBe` ["Fish", "Eggs", "Salmon"]
        it "can format list into html elements" $ do
            let (Just ol) = orderedList "1. Fish\n2. Eggs\n3. Salmon"
            formatOrderedList ol `shouldBe`
                "<ol>\n<li>Fish</li>\n<li>Eggs</li>\n<li>Salmon</li>\n</ol>"
        it "doesn't create list item for non-formatted items" $ do
            let (Just res) = orderedList "1. Fish\n2 Eggs\n3. Salmon"
            getOrderedList res `shouldBe` ["Fish\n2 Eggs", "Salmon"]
        it "must have no spaces after newline" $ do
            let (Just res) = orderedList "1. Fish\n 2. Eggs\n3. Salmon"
            getOrderedList res `shouldBe` ["Fish\n 2. Eggs", "Salmon"]
        it "doesn't create separate list items for inlined items" $ do
            let (Just res) = orderedList "1. Fish 2. Eggs 3. Salmon"
            getOrderedList res `shouldBe` ["Fish 2. Eggs 3. Salmon"]
        it "returns Nothing if input doesn't lead with number" $ do
            orderedList "Fish\n2. Eggs\n3. Salmon" `shouldBe` Nothing
        it "does not matter what numbers are" $ do
            let (Just res) =
                    orderedList "123293920. Fish\n9. Eggs\n01201. Salmon"
            getOrderedList res `shouldBe` ["Fish", "Eggs", "Salmon"]
    describe "Headers" $ do
        it "converts markdown headers to html headers" $ do
            let (Just res) = headers "### I love fish tacos"
            getHeaders res `shouldBe` "<h3>I love fish tacos</h3>"
        it "can convert headers of different sizes" $ do
            let (Just res) = headers "##### I love fish tacos"
            getHeaders res `shouldBe` "<h5>I love fish tacos</h5>"
        it "does not go further than 6 headers" $ do
            headers "######## I love fish tacos" `shouldBe` Nothing
        it "returns Nothing when input lacks whitespace buffer" $ do
            headers "###I love fish tacos" `shouldBe` Nothing
        it "returns Nothing when given an empty string" $ do
            headers "" `shouldBe` Nothing
        it "returns Nothing when input does not lead with # sign" $ do
            headers " ### I love fish tacos" `shouldBe` Nothing
        it "returns Nothing when input is whitespace" $ do
            headers " " `shouldBe` Nothing
    describe "FormattedWords" $ do
        it "returns words wrapped in '*' to <em> tags" $ do
            let res = formattedWords "I *love* fish tacos."
            getFormattedWords res `shouldBe` "I <em>love</em> fish tacos."
        it "converts separately emphasized words" $ do
            let res = formattedWords "I *love* fish *tacos*."
            getFormattedWords res `shouldBe`
                "I <em>love</em> fish <em>tacos</em>."
        it "converts multiple words" $ do
            let res = formattedWords "I *love fish* tacos."
            getFormattedWords res `shouldBe` "I <em>love fish</em> tacos."
        it "converts emphasis mid-word" $ do
            let res = formattedWords "I lo*ve fi*sh tacos."
            getFormattedWords res `shouldBe` "I lo<em>ve fi</em>sh tacos."
        it "converts words wrapped in '**' to <strong> tags" $ do
            let res = formattedWords "I **love** fish tacos."
            getFormattedWords res `shouldBe`
                "I <strong>love</strong> fish tacos."
        it "converts separately strong words" $ do
            let res = formattedWords "I **love** fish **tacos**."
            getFormattedWords res `shouldBe`
                "I <strong>love</strong> fish <strong>tacos</strong>."
        it "converts multiple strong words" $ do
            let res = formattedWords "I **love fish** tacos."
            getFormattedWords res `shouldBe`
                "I <strong>love fish</strong> tacos."
        it "converts strongs mid-word" $ do
            let res = formattedWords "I lo**ve fi**sh tacos."
            getFormattedWords res `shouldBe`
                "I lo<strong>ve fi</strong>sh tacos."
        it "converts em nested inside strong" $ do
            let res = formattedWords "**I *love* fish** tacos."
            getFormattedWords res `shouldBe`
                "<strong>I <em>love</em> fish</strong> tacos."
        it "converts strong nested inside em" $ do
            let res = formattedWords "*I **love** fish* tacos."
            getFormattedWords res `shouldBe`
                "<em>I <strong>love</strong> fish</em> tacos."
        it "converts a combination of both" $ do
            let res = formattedWords "I *lo**ve** fi*sh tacos."
            getFormattedWords res `shouldBe`
                "I <em>lo<strong>ve</strong> fi</em>sh tacos."
        it "returns Nothing when no emphases exist" $ do
            let res = formattedWords "I love fish tacos."
            getFormattedWords res `shouldBe` "I love fish tacos."
    describe "Paragraphs" $ do
        it "converts single lines to paragraphs" $ do
            let res = paragraphs "I love fish tacos."
            getParagraphs res `shouldBe` "<p>I love fish tacos.</p>"
        it "keeps single line breaks within same p tags" $ do
            let res = paragraphs "I love fish tacos.\nThey're really great!"
            getParagraphs res `shouldBe`
                "<p>I love fish tacos. They're really great!</p>"
        it "puts double line breaks into new paragraph, separated by line break" $ do
            let res = paragraphs "I love fish tacos.\n\nThey're really great!"
            getParagraphs res `shouldBe`
                "<p>I love fish tacos.</p>\n<p>They're really great!</p>"
        it "applies any additional line breaks between paragraphs" $ do
            let res = paragraphs "I love fish tacos.\n\n\nThey're really great!"
            getParagraphs res `shouldBe`
                "<p>I love fish tacos.</p>\n<p>They're really great!</p>"
        it "ignores additional newlines" $ do
            let res =
                    paragraphs
                        "I love fish tacos.\n\n\n\n\n\nThey're really great!"
            getParagraphs res `shouldBe`
                "<p>I love fish tacos.</p>\n<p>They're really great!</p>"

module InitializersSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.QuickCheck hiding (orderedList)

import Initializers.Headers
import Initializers.OrderedList
import Initializers.UnorderedList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "UnorderedList" $ do
        it "wraps an array of list items" $ do
            let (Just res) = unorderedList "* Fish\n* Eggs\n* Salmon\n* Cake"
            getUnorderedList res `shouldBe` ["Fish", "Eggs", "Salmon", "Cake"]
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

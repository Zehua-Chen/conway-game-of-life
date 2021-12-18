module Conway.IO where

data Cell = Live | Dead

newtype Page = Page {livingCells :: [Cell]}

newtype Story = Story {pages :: [Page]}

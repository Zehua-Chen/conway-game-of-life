module Schema where

data Cell = Live | Dead

data Page = Page { livingCells :: [Cell] }

data Story = Story { pages :: [Page] }

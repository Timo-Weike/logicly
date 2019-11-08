module Text.Table
    (
        headerAndContentToTable
    )
where

import Data.List
import Text.PrettyPrint.Boxes

headerAndContentToTable :: [String] -> [[String]] -> String
headerAndContentToTable header con = render $ hsep 0 top $ putHeader <$> h_and_con_t
    where
        con_t = transpose con
        boxed_content = vcat center2 . (text <$>) <$> con_t
        h_and_con_t = intersperse ("", emptyBox 1 1) $ zip header boxed_content

        putHeader :: (String, Box) -> Box
        putHeader (h, box) = vcat center1 [h_box, sep_box, box]
            where
                h_box = text h
                h_width = cols h_box
                con_width = cols box
                max_width = max h_width con_width
                sep_box = text $ replicate max_width '-'



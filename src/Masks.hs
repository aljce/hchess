module Masks where

import Prelude

import Data.Word

import Data.Vector.Unboxed

type Mask = Word64

rank1M, rank2M, rank3M, rank4M, rank5M, rank6M, rank7M, rank8M :: Mask
rank1M = 0x00000000000000FF
rank2M = 0x000000000000FF00
rank3M = 0x0000000000FF0000
rank4M = 0x00000000FF000000
rank5M = 0x000000FF00000000
rank6M = 0x0000FF0000000000
rank7M = 0x00FF000000000000
rank8M = 0xFF00000000000000

rankMasks :: Vector Word64
rankMasks = fromList [rank1M,rank2M,rank3M,rank4M,rank5M,rank6M,rank7M,rank8M]

fileAM, fileBM, fileCM, fileDM, fileEM, fileFM, fileGM, fileHM :: Mask
fileAM = 0x0101010101010101
fileBM = 0x0202020202020202
fileCM = 0x0404040404040404
fileDM = 0x0808080808080808
fileEM = 0x1010101010101010
fileFM = 0x2020202020202020
fileGM = 0x4040404040404040
fileHM = 0x8080808080808080

fileMasks :: Vector Mask
fileMasks = fromList [fileAM,fileBM,fileCM,fileDM,fileEM,fileFM,fileGM,fileHM]

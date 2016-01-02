module Masks where 

import Data.Word

import Data.Vector.Unboxed

type Mask = Word64

rank1M, rank2M, rank3M, rank4M, rank5M, rank6M, rank7M, rank8M :: Mask
rank1M = 0xFFFFFFFFFFFFFF00
rank2M = 0xFFFFFFFFFFFF00FF
rank3M = 0xFFFFFFFFFF00FFFF
rank4M = 0xFFFFFFFF00FFFFFF
rank5M = 0xFFFFFF00FFFFFFFF
rank6M = 0xFFFF00FFFFFFFFFF
rank7M = 0xFF00FFFFFFFFFFFF
rank8M = 0x00FFFFFFFFFFFFFF

rankMasks :: Vector Word64 
rankMasks = fromList [rank1M,rank2M,rank3M,rank4M,rank5M,rank6M,rank7M,rank8M]

fileAM, fileBM, fileCM, fileDM, fileEM, fileFM, fileGM, fileHM :: Mask
fileAM = undefined
fileBM = undefined
fileCM = undefined
fileDM = undefined
fileEM = undefined
fileFM = undefined
fileGM = undefined 
fileHM = undefined 

fileMasks :: Vector Mask 
fileMasks = fromList [fileAM,fileBM,fileCM,fileDM,fileEM,fileFM,fileGM,fileHM]

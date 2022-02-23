module Main where

import Prelude

import Control.Monad
import Criterion
import Criterion.Main

import Env
import Rib
import Utils
import VM

main :: IO ()
main = do
  symTblHello <- makeSymTable helloWorldStr
  symTblMinRepl <- makeSymTable minReplStr
  symTblFib10 <- makeSymTable fib10Str
  symTblFib20 <- makeSymTable fib20Str
  symTblFib30 <- makeSymTable fib30Str
  defaultMain
    [ bgroup "RVM"
      [ bgroup "decoding symbol table"
        [ bench "HELLO!"   . whnfIO $ makeSymTable helloWorldStr
        , bench "Min REPL" . whnfIO $ makeSymTable minReplStr
        ]
      , bgroup "decoding instructions"
        [ bench "HELLO!"   . whnfIO $ makeInstructions symTblHello helloWorldStr
        , bench "Min REPL" . whnfIO $ makeInstructions symTblMinRepl minReplStr
        , bench "fib 30"   . whnfIO $ makeInstructions symTblFib30 fib30Str
        ]
      , bgroup "eval"
        [ bench "fib 10" . whnfIO $ runProgram fib10Str
        , bench "fib 20" . whnfIO $ runProgram fib20Str
        , bench "fib 30" . whnfIO $ runProgram fib30Str
        ]
      ]
    ]
  where
    benchDecodeInstr str symTbl = do
      setStack (RibInt 0)
      decodeInstructions symTbl str

makeSymTable :: String -> IO Rib
makeSymTable inputStr = initialSymbolTable symbolTableStr emptySymbolsCount
  where
    (symbolTableStr, emptySymbolsCount, instructionsStr) = splitBytecode inputStr

makeInstructions :: Rib -> String -> IO Rib
makeInstructions symTbl inputStr = do
  setStack (RibInt 0)
  decodeInstructions symTbl instructionsStr
  where
    (symbolTableStr, emptySymbolsCount, instructionsStr) = splitBytecode inputStr

runProgram :: String -> IO ()
runProgram programStr = do
  instructions <- initialize programStr
  eval instructions

splitBytecode :: String -> (String, Int, String)
splitBytecode inputStr =
  let (start, end) = span (/= ';') inputStr
      (symTbl, emptySymCount) = readInt start 0
  in (symTbl, emptySymCount, drop 1 end)

helloWorldStr :: String
helloWorldStr = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" -- RVM code that prints HELLO!

minReplStr :: String
minReplStr = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,,-,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn5^~i$#`kn5^~i$#`kn5^~i$#`kn5^~RL^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!93_@J^{]%3uy]?'i$9?C_@J^G^~F^z]I'i$'i$9IC^@YGG^~F^@JvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^3vL@ZIC^@YGG^@JvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^3vE@Z?i%YD^@JvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Lc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FKKvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$4_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHKYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&K`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Kl^~I_k|]L9Ji&`^{]A'^9AKl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8ALaL_~YABaB_~YAHaH_~R`~R_'^~^?`^{]$(i$^z!:9>'i$(bL^~R^zz!S.Kmk!S0Klk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!5:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!4:lkv2!N:lkv3]&:lkv4!S#:lkv5!3:lkv6y"

fib10Str :: String
fib10Str = ".;'u!'*>?m_>?l^,l~Bk^zD@ki#!,:lkl!-:lkm!):lku!+:lkv0!*:lkv1!(:lkv2y"

fib20Str :: String
fib20Str = ".;'v7!'*>?m_>?l^,l~Bk^zD@ki#!,:lkl!-:lkm!):lku!+:lkv0!*:lkv1!(:lkv2y"

fib30Str :: String
fib30Str = ".;'vA!'*>?m_>?l^,l~Bk^zD@ki#!,:lkl!-:lkm!):lku!+:lkv0!*:lkv1!(:lkv2y"
